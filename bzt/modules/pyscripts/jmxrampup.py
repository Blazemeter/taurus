"""
Copyright 2024 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

import os
import sys
import socket
import traceback
import time
from math import floor
from collections import deque
from multiprocessing import AuthenticationError
from multiprocessing.connection import Client
import logging

logging.basicConfig(stream=sys.stdout, level=logging.WARNING,
                    format="%(asctime)s %(levelname)s: %(message)s")

class JmeterRampupProcess(object):
    def __init__(self, beanshells, rampup_addr, rampup_port, rampup_pass, artifacts_dir, log_level=logging.INFO):
        self.beanshells = beanshells
        self.rampup_addr = rampup_addr
        self.rampup_port = rampup_port
        self.rampup_pass = rampup_pass
        self.filename = os.path.join(artifacts_dir, "kpi.jtl")
        self.log = logging.getLogger("Rampup.Process")
        self.log.setLevel(log_level)

    def run(self):
        res_list = None
        cur_goal = None
        while True:
            new_rampup = self._check_change_params()
            if new_rampup:
                self.log.info(f"Got new rampup configuration: {new_rampup}")
                res_list = self._calc_rampup(self.filename, new_rampup)
                cur_goal = res_list.popleft() if res_list else None
            if cur_goal:
                now = self._now()
                do_goal = None
                while cur_goal and cur_goal[1] < now:
                    do_goal = cur_goal
                    cur_goal = res_list.popleft() if res_list else None
                if do_goal:
                    self.log.info(f"Setting concurrency: {do_goal}")
                    self._change_concurrencies(do_goal[0])
            if self._stop():
                break
            time.sleep(0.5)

    def _now(self):
        return time.time()

    def _stop(self):
        return False

    def _socket_recv(self, sock_a):
        read = ""
        try:
            for _ in range(0, 1000):  # just safety valve
                read += sock_a.recv(4096).decode()
        except socket.timeout:
            self.log.debug("Timeout reading from beanshell socket")
        except BaseException:
            self.log.warning("Failed to read response from beanshell server %s: %s" %
                  (sock_a.getpeername(), traceback.format_exc()))

        lines = read.splitlines(True)
        result = []
        for line in lines:
            if line.startswith("BeanShell "):
                continue

            while line.startswith("bsh % "):
                line = line[len("bsh % "):]

            line = line.strip()
            result.append(line)

        return "".join(result)

    def _get_current_concurrency(self, file):
        try:
            with open(file, 'rb') as f:
                try:
                    f.seek(-1, os.SEEK_END)
                    # Find last completed line (ending with \n)
                    while f.read(1) != b'\n':
                        f.seek(-2, os.SEEK_CUR)
                    f.seek(-2, os.SEEK_CUR)
                    while f.read(1) != b'\n':
                        f.seek(-2, os.SEEK_CUR)
                except OSError:
                    f.seek(0)
                last_line = f.readline().decode()
            try:
                cur_con = last_line.split(",")[-5]
            except IndexError:
                self.log.warning("Could not read users retry next loop")
                cur_con = False
        except FileNotFoundError:
            self.log.warning(f"File {self.filename} not found. Retry next loop")
            cur_con = False

        self.log.info(f"Current concurrency: {cur_con}")
        return cur_con

    def _change_concurrencies(self, desired_users):
        for beanshell_addr, beanshell_port in self.beanshells:
            self._change_concurrency(desired_users, beanshell_addr, beanshell_port)

    def _change_concurrency(self, desired_users, beanshell_addr, beanshell_port):
        result = ""
        socket_factory = socket.socket
        sock = socket_factory(socket.AF_INET, socket.SOCK_STREAM)
        try:
            sock.connect((beanshell_addr, beanshell_port + 1))
            sock.settimeout(1)
            script = (f'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                      f'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","{desired_users}");')
            sock.sendall(script.encode())

            result += self._socket_recv(sock) + "\n"
        except BaseException:
            self.log.warning("Failed to send command to beanshell server %s:%s: %s" %
                  (beanshell_addr, beanshell_port, traceback.format_exc()))
        finally:
            sock.close()
        self.log.debug(f"Beanshell recv: {result}")

    def _check_change_params(self):
        address = (self.rampup_addr, self.rampup_port)

        try:
            with Client(address, authkey=self.rampup_pass.encode()) as conn:
                return conn.recv()
        except ConnectionRefusedError:
            pass
        except AuthenticationError:
            self.log.fatal("Forbidden. Rampup server returns AuthenticationError.")
            print(f"Forbidden. Rampup server returns AuthenticationError: {traceback.format_exc()}",
                  file=sys.stderr, flush=True)
            sys.exit(1)

        return None

    def _calc_rampup(self, file, new_rampup):
        users = new_rampup["concurrency"]
        if not users:
            return None
        rampup = new_rampup["ramp_up_duration"]
        steps = new_rampup["ramp_up_steps"]

        cur_users = self._get_current_concurrency(file)
        try:
            cur_users = int(cur_users)
        except ValueError:
            cur_users = 1

        users_list = deque()
        cur_time = round(self._now())

        if not rampup:
            # Ramp-up immediately
            rampup = 0
            steps = 1
        if not steps:
            steps = max(abs(users - cur_users), 1)
        ramp_in_sec = rampup * 60
        step_time = ramp_in_sec / steps
        users_pers_step = (users - cur_users)/steps

        prev_concurrency = cur_users
        for i in range(steps):
            concurrency = cur_users + (floor(users_pers_step*(i+1) + 0.5))
            if concurrency != prev_concurrency:
                users_list.append((concurrency, cur_time + step_time*i))
                prev_concurrency = concurrency
        self.log.info(f'Rampup plan: {users_list}')
        return users_list

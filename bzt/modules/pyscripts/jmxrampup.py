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
import socket
import traceback
import time
from math import floor
from collections import deque
from multiprocessing.connection import Client
import logging


class JmeterRampupProcess(object):
    def __init__(self, beanshells, rampup_addr, rampup_port, rampup_pass, artifacts_dir):
        self.beanshells = beanshells
        self.rampup_addr = rampup_addr
        self.rampup_port = rampup_port
        self.rampup_pass = rampup_pass
        self.filename = os.path.join(artifacts_dir, "kpi.jtl")

    def run(self):
        res_list = None
        cur_goal = None
        while True:
            new_rampup = self._check_change_params()
            if new_rampup:
                res_list = self._calc_rampup(self.filename, new_rampup)
                cur_goal = res_list.popleft()
            if cur_goal:
                if cur_goal[1] < time.time():
                    self._change_concurrency(cur_goal[0])
                    try:
                        cur_goal = res_list.popleft()
                    except IndexError:
                        cur_goal = None
            time.sleep(0.5)

    def _socket_recv(self, sock_a):
        read = ""
        try:
            for _ in range(0, 1000):  # just safety valve
                read += sock_a.recv(4096).decode()
        except socket.timeout:
            print("Timeout reading from beanshell socket")
        except BaseException:
            logging.warning("Failed to read response from beanshell server %s: %s",
                            sock_a.getpeername(), traceback.format_exc())

        lines = read.splitlines(True)
        result = []
        for line in lines:
            if line.startswith("BeanShell "):
                continue

            while line.startswith("bsh % "):
                line = line[len("bsh % "):]
            result.append(line)

        return "".join(result)

    def _get_current_concurrency(self, file):
        with open(file, 'rb') as f:
            try:
                f.seek(-2, os.SEEK_END)
                while f.read(1) != b'\n':
                    f.seek(-2, os.SEEK_CUR)
            except OSError:
                f.seek(0)
            last_line = f.readline().decode()
        try:
            cur_con = last_line.split(",")[-5]
        except IndexError:
            print("Could not read users retry next loop")
            cur_con = False
        return cur_con

    def _change_concurrency(self, desired_users):
        for beanshell_addr, beanshell_port in self.beanshells:
            self.__change_concurrency(desired_users, beanshell_addr, beanshell_port)

    def __change_concurrency(self, desired_users, beanshell_addr, beanshell_port):
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
            print("Failed to send command to beanshell server %s:%s: %s",
                  beanshell_addr, beanshell_port,traceback.format_exc())
        finally:
            sock.close()
        print(result)

    def _check_change_params(self):
        address = (self.rampup_addr, self.rampup_port)

        try:
            with Client(address, authkey=self.rampup_pass.encode()) as conn:
                return conn.recv()
        except ConnectionRefusedError:
            pass
        return None

    def _calc_rampup(self, file, new_rampup):
        users = new_rampup["concurrency"]
        rampup = new_rampup["ramp_up_duration"]
        steps = new_rampup["ramp_up_steps"]
        ramp_in_sec = rampup * 60
        cur_users = self._get_current_concurrency(file)
        users_list = deque()

        step_time = ramp_in_sec / steps
        users_pers_step = (users - int(cur_users))/steps
        cur_time = round(time.time())
        for i in range(steps):
            users_list.append((int(cur_users) + (floor(users_pers_step*(i+1) + 0.5)), cur_time + step_time*i))
        print(users_list)
        return users_list

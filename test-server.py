import signal
from multiprocessing import connection


def timeout_handler(signum, frame):
    raise TimeoutError("Operation timed out")


def handle_commands():
    command = {"cmd": "concurrency_change",
               "args": {
                   "concurrency": 2,
                   "rampup_steps": 2,
                   "rampup_duration": 1
               }}
    if command['cmd'] == 'concurrency_change':
        args = command['args']
        cc = args['concurrency']
        ramp_up_steps = args['rampup_steps']
        ramp_up_duration = args['rampup_duration']
        print(f"Got concurrency_change command with info cc : {cc} . duration : {ramp_up_duration}. steps : {ramp_up_steps}")
        signal.signal(signal.SIGALRM, timeout_handler)
        signal.alarm(3)
        try:
            listener = connection.Listener(('localhost', 6000), authkey=b'some_key')
            conn = listener.accept()
            data_to_send = {'ramp_up_duration': ramp_up_duration,
                            'ramp_up_steps': ramp_up_steps, 'concurrency': cc}
            conn.send(data_to_send)
            conn.close()
        except TimeoutError:
            print('timeout reached when attempting to communicate with taurus')
        finally:
            print('alarm to communicate with taurus released')
        signal.alarm(0)
        listener.close()


if __name__ == '__main__':
    handle_commands()

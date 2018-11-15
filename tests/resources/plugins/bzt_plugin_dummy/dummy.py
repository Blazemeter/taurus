from bzt.engine import ScenarioExecutor


class DummyExecutor(ScenarioExecutor):
    def __init__(self):
        super(DummyExecutor, self).__init__()
        self.n_times = 5

    def prepare(self):
        self.log.info("I am a dummy executor preparing for work")

    def startup(self):
        self.log.info("I am a dummy executor starting to work")

    def check(self):
        self.log.info("I am a dummy executor checking if finished (%d)", self.n_times)
        self.n_times -= 1
        return self.n_times == 0

    def has_results(self):
        self.log.info("I am a dummy executor checking if i have test results")
        return True

    def shutdown(self):
        self.log.info("I am a dummy executor shutting down")

    def post_process(self):
        self.log.info("I am a dummy executor post-processing my work")


"""
Implementations for `Provisioning` classes
"""
# TODO: allow sequential executions?

from bzt.engine import Provisioning


class Local(Provisioning):
    """
    Local provisioning means we start all the tools locally
    """

    def prepare(self):
        """
        Call prepare on executors
        """
        super(Local, self).prepare()
        for executor in self.executors:
            self.log.debug("Preparing executor: %s", executor)
            executor.prepare()

    def startup(self):
        """
        Call startup on executors
        """
        for executor in self.executors:
            self.log.debug("Startup %s", executor)
            executor.startup()

    def check(self):
        """
        Check executors for finish. Return True if all of them has finished.
        """
        return self.__class__.check_modules_list(self.executors, True)

    def shutdown(self):
        """
        Call shutdown on executors
        """
        for executor in self.executors:
            self.log.debug("Shutdown %s", executor)
            executor.shutdown()

    def post_process(self):
        """
        Post-process executors
        """
        for executor in self.executors:
            self.log.debug("Post-process %s", executor)
            executor.post_process()


class FileLister(object):
    """
    A mixin to get required files info from executor
    """

    def resource_files(self):
        """
        Get list of resource files

        :rtype: list
        """
        raise NotImplementedError()

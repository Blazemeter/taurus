class TaurusException(BaseException):
    pass


class TaurusConfigError(TaurusException):
    pass


class InvalidTaurusConfiguration(TaurusConfigError):
    pass


class TaurusInternalException(TaurusException):
    pass


class ToolError(TaurusException):
    def __init__(self, message, diagnostics=None):
        """
        :type message: str
        :type diagnostics: list[str]
        """
        super(ToolError, self).__init__(message)
        self.diagnostics = diagnostics


class TaurusNetworkError(TaurusException):
    pass

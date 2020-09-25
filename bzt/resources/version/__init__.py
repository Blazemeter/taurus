try:
    from .version import VERSION
except ImportError:
    VERSION = "DEV"

try:
    from .gitinfo import GIT_INFO
except ImportError:
    GIT_INFO = "DEV"

try:
    from .build import BUILD_NUM
except ImportError:
    BUILD_NUM = "DEV"

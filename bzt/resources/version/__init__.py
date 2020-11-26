try:
    from .version import VERSION
except ImportError:
    VERSION = "1.15.1"

try:
    from .gitinfo import GIT_INFO
except ImportError:
    GIT_INFO = "DEV"

try:
    from .build import BUILD
except ImportError:
    BUILD = "DEV"

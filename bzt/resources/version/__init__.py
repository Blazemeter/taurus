DEV_VERSION = "DEV"

try:
    from .version import VERSION
except ImportError:
    VERSION = DEV_VERSION

try:
    from .gitinfo import GIT_INFO
except ImportError:
    GIT_INFO = DEV_VERSION

try:
    from .build import BUILD
except ImportError:
    BUILD = DEV_VERSION

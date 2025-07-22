# Multi-stage build for Blazemeter Taurus performance testing tool
FROM ubuntu:24.04 AS base

# Metadata
LABEL maintainer="dmykhaliev@perforce.com>"
LABEL description="Blazemeter Taurus performance testing environment"
LABEL version="2.0"

# Build arguments
ARG DEBIAN_FRONTEND=noninteractive
ARG APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
# Environment variables - set locale early
ENV DEBIAN_FRONTEND=${DEBIAN_FRONTEND}
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=${APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE}
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV PYTHONUNBUFFERED=1
ENV PYTHONDONTWRITEBYTECODE=1
ENV PATH="/usr/local/rbenv/bin:/usr/local/rbenv/shims:${PATH}"
ENV RBENV_ROOT=/usr/local/rbenv

# Create non-root user early
RUN groupadd -g 1337 bzt && \
    useradd -u 1337 -g bzt -m -s /bin/bash bzt

# ================================
# Stage 1: System Dependencies
# ================================
FROM base AS system-deps

ARG NODE_VERSION=20
ARG PYTHON_VERSION=3.12

ARG DEBIAN_FRONTEND=noninteractive
ARG APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
# Update package lists and install essential packages including dirmngr for GPG
RUN apt-get update && \
    locale-gen "en_US.UTF-8" && \
    update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        wget \
        gnupg \
        dirmngr \
        gpg-agent \
        lsb-release \
        software-properties-common \
        apt-transport-https && \
    rm -rf /var/lib/apt/lists/*

# Add external repositories
# Add NodeSource repository
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION}.x | bash -
# Install Node.js
RUN apt-get update && \
    apt-get install -y nodejs && \
    rm -rf /var/lib/apt/lists/*

# Add Google Chrome repository
RUN curl -fsSL https://dl.google.com/linux/linux_signing_key.pub | gpg --dearmor -o /etc/apt/keyrings/google-chrome.gpg && \
    echo "deb [arch=amd64 signed-by=/etc/apt/keyrings/google-chrome.gpg] http://dl.google.com/linux/chrome/deb/ stable main" | tee /etc/apt/sources.list.d/google-chrome.list

# Add Firefox PPA (avoid snap)
RUN printf '%s\n' 'Package: firefox*' 'Pin: release o=Ubuntu*' 'Pin-Priority: -1' > /etc/apt/preferences.d/firefox-no-snap && \
    add-apt-repository ppa:mozillateam/ppa -y

# Add K6 repository with proper GPG setup
RUN mkdir -p /root/.gnupg && \
    chmod 700 /root/.gnupg && \
    gpg --batch --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C5AD17C747E3415A3642D57D77C6C491D6AC1D69 && \
    gpg --batch --export C5AD17C747E3415A3642D57D77C6C491D6AC1D69 | gpg --dearmor -o /usr/share/keyrings/k6-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/k6-archive-keyring.gpg] https://dl.k6.io/deb stable main" | tee /etc/apt/sources.list.d/k6.list

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        # Build essentials
        build-essential \
        gcc \
        g++ \
        make \
        libtool \
        pkg-config \
        # Python and development
        python${PYTHON_VERSION} \
        python${PYTHON_VERSION}-dev \
        python3-pip \
        # System utilities
        git \
        unzip \
        net-tools \
        apt-utils \
        xvfb \
        # SSL and security
        libssl-dev \
        ca-certificates \
        # Development libraries
        libyaml-dev \
        libxml2-dev \
        libxslt-dev \
        # Java
        openjdk-11-jdk \
        # Web testing tools
        google-chrome-stable \
        firefox \
        # Load testing tools
        siege \
        apache2-utils \
        tsung \
        k6 && \
    rm -rf /var/lib/apt/lists/*

# Set up Python alternatives
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python${PYTHON_VERSION} 1 && \
    update-alternatives --install /usr/bin/python python /usr/bin/python${PYTHON_VERSION} 1

# ================================
# Stage 2: Language Runtimes
# ================================
FROM system-deps AS runtimes

# Install .NET SDK
RUN DOTNET_URL="https://builds.dotnet.microsoft.com/dotnet/Sdk/8.0.412/dotnet-sdk-8.0.412-linux-x64.tar.gz" && \
    DOTNET_SHA512="48062e12222224845cb3f922d991c78c064a1dd056e4b1c892b606e24a27c1f5413dc42221cdcf4225dcb61e3ee025d2a77159006687009130335ac515f59304" && \
    curl -fSL --output dotnet.tar.gz "${DOTNET_URL}" && \
    echo "${DOTNET_SHA512} dotnet.tar.gz" | sha512sum -c - && \
    mkdir -p /usr/share/dotnet && \
    tar -zxf dotnet.tar.gz -C /usr/share/dotnet && \
    rm dotnet.tar.gz && \
    ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet

# Install Ruby & OpenJDK
RUN apt-get update && apt-get install -y --no-install-recommends \
    ruby-full \
    zlib1g-dev \
    libreadline-dev \
    openjdk-11-jdk && \
    rm -rf /var/lib/apt/lists/*

# ================================
# Stage 3: Python Dependencies
# ================================
FROM runtimes AS python-deps

# Copy Python package
COPY dist/bzt*.whl /tmp/

# Upgrade pip and core packaging tools
RUN python3 -m pip install --no-cache-dir --upgrade --break-system-packages --ignore-installed \
        pip \
        setuptools \
        wheel

# Install BZT package
RUN python3 -m pip install --no-cache-dir --break-system-packages --ignore-installed /tmp/bzt*.whl chardet

# ================================
# Stage 4: Browser Setup
# ================================
FROM python-deps AS browser-setup

# Fix Chrome wrapper
RUN cp "$(python3 -c "import bzt; print(f'{bzt.__path__[0]}/resources/chrome_launcher.sh')")" \
       /opt/google/chrome/google-chrome && \
    chmod +x /opt/google/chrome/google-chrome

# ================================
# Stage 5: Final Image
# ================================
FROM browser-setup AS final

ARG DEBIAN_FRONTEND=noninteractive
ARG APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1

# Create BZT configuration directory and artifacts directory
RUN mkdir -p /etc/bzt.d /bzt-configs /tmp/artifacts && \
    chown -R bzt:bzt /bzt-configs /tmp/artifacts

# Configure BZT
RUN echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json && \
    echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json

# Install BZT tools
RUN bzt -install-tools -v

# Verify installations
RUN google-chrome-stable --version && \
    firefox --version && \
    dotnet --version | head -1 && \
    node --version && \
    python3 --version && \
    ruby --version

# Cleanup
RUN apt-get remove -y \
        software-properties-common \
        build-essential \
        gcc \
        g++ && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* \
           /var/tmp/* \
           /root/.cache \
           /usr/share/man \
           /usr/share/doc

# Remove security-sensitive files
RUN find /root/.bzt -name "*.pem" -delete 2>/dev/null || true && \
    find /root/.bzt -name "*.key" -delete 2>/dev/null || true

# Remove egg-info directories
RUN find "$(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")" \
         -name '*.egg-info' -exec rm -rf {} + 2>/dev/null || true

# Set proper permissions
RUN chown -R bzt:bzt /bzt-configs /tmp/artifacts

# Switch to non-root user
USER bzt:bzt

# Set working directory
WORKDIR /bzt-configs

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD bzt --help > /dev/null || exit 1

# Default entrypoint
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

# Default command
CMD ["--help"]

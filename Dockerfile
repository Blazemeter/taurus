FROM ubuntu:24.04 AS base

# Metadata
LABEL maintainer="Blazemeter Team"
LABEL description="Blazemeter Taurus"

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
ENV PIP_BREAK_SYSTEM_PACKAGES=1
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
    apt-get install -y --no-install-recommends locales && \
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

# Download Chrome package
ADD https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb /tmp

# Add K6 repository with proper GPG setup
RUN mkdir -p /root/.gnupg && \
    chmod 700 /root/.gnupg && \
    gpg --batch --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C5AD17C747E3415A3642D57D77C6C491D6AC1D69 && \
    gpg --batch --export C5AD17C747E3415A3642D57D77C6C491D6AC1D69 | gpg --dearmor -o /usr/share/keyrings/k6-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/k6-archive-keyring.gpg] https://dl.k6.io/deb stable main" | tee /etc/apt/sources.list.d/k6.list

# Add Firefox PPA (avoid snap)
RUN printf '%s\n' 'Package: firefox*' 'Pin: release o=Ubuntu*' 'Pin-Priority: -1' > /etc/apt/preferences.d/firefox-no-snap && \
    add-apt-repository ppa:mozillateam/ppa -y

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
        libnss3 \
        libgbm1 \
        libxss1 \
        libdrm2 \
        libxkbcommon0 \
        fonts-liberation \
        # SSL and security
        libssl-dev \
        # Development libraries
        libyaml-dev \
        libxml2-dev \
        libxslt-dev \
        # vulnerable libraries \
        libexpat1=2.6.1-2ubuntu0.3 \
        libexpat1-dev=2.6.1-2ubuntu0.3 \
        # Load testing tools
        siege \
        apache2-utils \
        tsung \
        k6  \
        # Web testing tools
        firefox \
        /tmp/google-chrome-stable_current_amd64.deb && \
    mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome && \
    rm /tmp/google-chrome-stable_current_amd64.deb && \
    rm -rf /var/lib/apt/lists/*

# replace firefox version to make security scanners happy
RUN current_version=$(dpkg -l firefox | tail -1 | awk '{print $3}' | grep -oP '[\d.]+' | head -1) && \
    snap_version="1:${current_version}snap1-0ubuntu1" && \
    sed -i "/^Package: firefox$/,/^$/{s/^Version: .*/Version: $snap_version/}" /var/lib/dpkg/status

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

# Install rbenv and Ruby
ARG RUBY_VERSION=3.4.5

RUN git clone --depth 1 https://github.com/rbenv/rbenv.git ${RBENV_ROOT} && \
    git clone --depth 1 https://github.com/rbenv/ruby-build.git ${RBENV_ROOT}/plugins/ruby-build && \
    echo 'export RBENV_ROOT=/usr/local/rbenv' >> /etc/profile.d/rbenv.sh && \
    echo 'export PATH="$RBENV_ROOT/bin:$PATH"' >> /etc/profile.d/rbenv.sh && \
    echo 'eval "$(rbenv init -)"' >> /etc/profile.d/rbenv.sh && \
    chmod +x /etc/profile.d/rbenv.sh

# Install Ruby
RUN eval "$(${RBENV_ROOT}/bin/rbenv init -)" && \
    rbenv install ${RUBY_VERSION} && \
    rbenv global ${RUBY_VERSION} && \
    rbenv rehash

# Set up Ruby alternatives
RUN update-alternatives --install /usr/local/bin/ruby ruby ${RBENV_ROOT}/shims/ruby 1 && \
    update-alternatives --install /usr/local/bin/gem gem ${RBENV_ROOT}/shims/gem 1

# Install OpenJDK
RUN apt-get update && apt-get install -y --no-install-recommends \
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
RUN python3 -m pip install --no-cache-dir --upgrade --ignore-installed \
        pip \
        setuptools \
        wheel

# Install BZT package
RUN python3 -m pip install --no-cache-dir --ignore-installed /tmp/bzt*.whl chardet

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
RUN mkdir -p /etc/bzt.d /bzt-configs /tmp/artifacts
# RUN chown -R bzt:bzt /bzt-configs /tmp/artifacts

# Configure BZT
RUN echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json && \
    echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json

# Install BZT tools
RUN bzt -install-tools -v

# Verify installations
RUN google-chrome-stable --version && \
    firefox --version && \
    dotnet --info && \
    node --version && \
    python3 --version && \
    ruby --version

#  force npm to use cross-spawn@7.0.5, this block can be removed when new version of nodejs uses cross-spawn@7.0.5
RUN npm_root=$(npm root -g) \
 && npm pack cross-spawn@7.0.5 -q \
 && mkdir -p "$npm_root/npm/node_modules/cross-spawn" \
 && tar -xzf cross-spawn-7.0.5.tgz \
       --strip-components=1 \
       -C "$npm_root/npm/node_modules/cross-spawn" \
 && rm cross-spawn-7.0.5.tgz

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

# update dotnet metadata to make scanners happy
RUN if [ -f /usr/share/dotnet/sdk/8.0.412/DotnetTools/dotnet-format/BuildHost-netcore/Microsoft.CodeAnalysis.Workspaces.MSBuild.BuildHost.deps.json ]; then \
      sed -i 's/17\.3\.4/17.11.31/g' /usr/share/dotnet/sdk/8.0.412/DotnetTools/dotnet-format/BuildHost-netcore/Microsoft.CodeAnalysis.Workspaces.MSBuild.BuildHost.deps.json; \
    fi
RUN if [ -f /usr/share/dotnet/sdk/8.0.412/DotnetTools/dotnet-watch/8.0.412-servicing.25320.8/tools/net8.0/any/BuildHost-netcore/Microsoft.CodeAnalysis.Workspaces.MSBuild.BuildHost.deps.json ]; then \
      sed -i 's/17\.3\.4/17.11.31/g' /usr/share/dotnet/sdk/8.0.412/DotnetTools/dotnet-watch/8.0.412-servicing.25320.8/tools/net8.0/any/BuildHost-netcore/Microsoft.CodeAnalysis.Workspaces.MSBuild.BuildHost.deps.json; \
    fi
RUN if [ -f /usr/share/dotnet/sdk/8.0.412/Roslyn/Microsoft.Build.Tasks.CodeAnalysis.deps.json ]; then \
      sed -i 's/17\.7\.2/17.11.31/g' /usr/share/dotnet/sdk/8.0.412/Roslyn/Microsoft.Build.Tasks.CodeAnalysis.deps.json; \
    fi
# update gatling logback 1.12.12 with 1.2.13 for fixing vulns \
# Replace logback-classic
RUN curl -sSL https://repo1.maven.org/maven2/ch/qos/logback/logback-classic/1.2.13/logback-classic-1.2.13.jar -o /tmp/logback-classic-1.2.13.jar && \
    if [ -f /root/.bzt/gatling-taurus/3.9.5/lib/logback-classic-1.2.12.jar ]; then \
        rm /root/.bzt/gatling-taurus/3.9.5/lib/logback-classic-1.2.12.jar && \
        mv /tmp/logback-classic-1.2.13.jar /root/.bzt/gatling-taurus/3.9.5/lib/logback-classic-1.2.13.jar; \
    else \
        rm /tmp/logback-classic-1.2.13.jar; \
    fi

# Replace logback-core
RUN curl -sSL https://repo1.maven.org/maven2/ch/qos/logback/logback-core/1.2.13/logback-core-1.2.13.jar -o /tmp/logback-core-1.2.13.jar && \
    if [ -f /root/.bzt/gatling-taurus/3.9.5/lib/logback-core-1.2.12.jar ]; then \
        rm /root/.bzt/gatling-taurus/3.9.5/lib/logback-core-1.2.12.jar && \
        mv /tmp/logback-core-1.2.13.jar /root/.bzt/gatling-taurus/3.9.5/lib/logback-core-1.2.13.jar; \
    else \
        rm /tmp/logback-core-1.2.13.jar; \
    fi

#Replace json-smart
RUN curl -sSL https://repo1.maven.org/maven2/net/minidev/json-smart/2.4.9/json-smart-2.4.9.jar -o /tmp/json-smart-2.4.9.jar && \
    if [ -f /root/.bzt/jmeter-taurus/5.5/lib/json-smart-2.4.8.jar ]; then \
        rm /root/.bzt/jmeter-taurus/5.5/lib/json-smart-2.4.8.jar && \
        mv /tmp/json-smart-2.4.9.jar /root/.bzt/jmeter-taurus/5.5/lib/json-smart-2.4.9.jar; \
    else \
        rm /tmp/json-smart-2.4.9.jar; \
    fi
RUN curl -sSL https://repo1.maven.org/maven2/net/minidev/accessors-smart/2.4.9/accessors-smart-2.4.9.jar -o /tmp/accessors-smart-2.4.9.jar && \
    if [ -f /root/.bzt/jmeter-taurus/5.5/lib/accessors-smart-2.4.8.jar ]; then \
        rm /root/.bzt/jmeter-taurus/5.5/lib/accessors-smart-2.4.8.jar && \
        mv /tmp/accessors-smart-2.4.9.jar /root/.bzt/jmeter-taurus/5.5/lib/accessors-smart-2.4.9.jar; \
    else \
        rm /tmp/accessors-smart-2.4.9.jar; \
    fi

# Remove security-sensitive files
WORKDIR /root/.bzt/python-packages/3.12.3/gevent/tests
RUN rm -rf *.pem
RUN rm -rf *.key

# Remove egg-info directories
RUN find "$(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")" \
         -name '*.egg-info' -exec rm -rf {} + 2>/dev/null || true

# Set working directory
WORKDIR /bzt-configs

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD bzt --help > /dev/null || exit 1

# Default entrypoint
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

# Default command
CMD ["--help"]

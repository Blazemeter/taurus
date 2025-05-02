FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"
ENV PIP_INSTALL="python3 -m pip install --no-cache-dir --break-system-packages "

ADD https://deb.nodesource.com/setup_18.x /tmp
ADD https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb /tmp
ADD https://packages.microsoft.com/config/ubuntu/21.04/packages-microsoft-prod.deb /tmp
COPY dist/bzt*whl /tmp

WORKDIR /tmp
# add node repo and call 'apt-get update'
RUN bash ./setup_18.x && $APT_INSTALL build-essential python3-pip python3.12-dev net-tools apt-utils

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.12 1

# Fix vulnerabilities / outdated versions
RUN $PIP_INSTALL --user --upgrade pip oauthlib pyjwt httplib2 numpy fonttools wheel

# install python packages..
RUN $PIP_INSTALL ./bzt*whl chardet

RUN $APT_UPDATE && $APT_INSTALL \
    unzip software-properties-common apt-transport-https \
    openjdk-11-jdk xvfb siege apache2-utils git make nodejs locales tsung libtool libssl-dev libyaml-dev libxml2-dev libxslt-dev

# Install .NET sdk
# check this page for the links and hash
# https://dotnetcli.azureedge.net/dotnet/release-metadata/8.0/releases.json
RUN curl -fSL --output dotnet.tar.gz https://download.visualstudio.microsoft.com/download/pr/ca6cd525-677e-4d3a-b66c-11348a6f920a/ec395f498f89d0ca4d67d903892af82d/dotnet-sdk-8.0.403-linux-x64.tar.gz \
    && dotnet_sha512='7aa03678228b174f51c4535f18348cdf7a5d35e243b1f8cb28a4a30e402e47567d06df63c8f6da4bdc3c7e898f54f4acc08d9952bfa49d3f220d0353253ac3e9' \
    && echo "$dotnet_sha512 dotnet.tar.gz" | sha512sum -c - \
    && mkdir -p /usr/share/dotnet \
    && tar -zxf dotnet.tar.gz -C /usr/share/dotnet \
    && rm dotnet.tar.gz \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet

# Install rbenv and ruby-build
SHELL ["/bin/bash", "-c"]
RUN git clone https://github.com/sstephenson/rbenv.git /usr/local/rbenv
RUN git clone https://github.com/sstephenson/ruby-build.git /usr/local/rbenv/plugins/ruby-build
RUN echo '# rbenv setup' > /etc/profile.d/rbenv.sh
RUN echo 'export RBENV_ROOT=/usr/local/rbenv' >> /etc/profile.d/rbenv.sh
RUN echo 'export PATH="$RBENV_ROOT/bin:$PATH"' >> /etc/profile.d/rbenv.sh
RUN echo 'eval "$(rbenv init -)"' >> /etc/profile.d/rbenv.sh
RUN chmod +x /etc/profile.d/rbenv.sh
RUN source /etc/profile.d/rbenv.sh \
    && rbenv install 3.3.3 && rbenv global 3.3.3 && rbenv rehash \
    && gem install rspec rake selenium-webdriver cgi:0.3.5 && gem update bundler date rexml && gem cleanup

RUN update-alternatives --install /usr/local/bin/ruby ruby /usr/local/rbenv/shims/ruby 1
RUN update-alternatives --install /usr/local/bin/gem gem /usr/local/rbenv/shims/gem 1
RUN update-alternatives --install /usr/local/bin/rspec rspec /usr/local/rbenv/shims/rspec 1

# firefox repo - do not use snap
RUN printf '%s\n' 'Package: firefox*' 'Pin: release o=Ubuntu*' 'Pin-Priority: -1' > /etc/apt/preferences.d/firefox-no-snap
RUN add-apt-repository ppa:mozillateam/ppa -y
RUN $APT_UPDATE && $APT_INSTALL firefox

# set en_US.UTF-8 as default locale
RUN locale-gen "en_US.UTF-8" && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8

# Get Google Chrome
RUN $APT_INSTALL ./google-chrome-stable_current_amd64.deb \
  && mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome

# Install K6
RUN $APT_INSTALL gpg-agent \
  && gpg -k \
  && gpg --no-default-keyring --keyring /usr/share/keyrings/k6-archive-keyring.gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C5AD17C747E3415A3642D57D77C6C491D6AC1D69 \
  && echo "deb [signed-by=/usr/share/keyrings/k6-archive-keyring.gpg] https://dl.k6.io/deb stable main" | tee /etc/apt/sources.list.d/k6.list \
  && $APT_UPDATE \
  && $APT_INSTALL k6

# auto installable tools
RUN mkdir -p /etc/bzt.d \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && cp `python3 -c "import bzt; print('{}/resources/chrome_launcher.sh'.format(bzt.__path__[0]))"` \
    /opt/google/chrome/google-chrome \
  && bzt -install-tools -v \
  && google-chrome-stable --version && firefox --version && dotnet --version | head -1

# Remove software-properties-common (used to install firefox from ppa) to fix vulnerabilities and clean up
RUN apt-get purge -y software-properties-common \
    && apt-get autoremove -y \
    && apt-get clean

### remove unused pem files
WORKDIR /root/.bzt/python-packages/3.10.12/gevent/tests
RUN rm -rf *.pem
RUN rm -rf *.key

RUN rm -rf /usr/share/javascript/jquery && rm -rf /usr/share/javascript/jquery-ui && rm -rf /tmp/* && mkdir /bzt-configs /tmp/artifacts

# Remove .egg-info directories to clean up
RUN find $(python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())") -name '*.egg-info' -exec rm -rf {} +

# Rootless user
# USER 1337:0
WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

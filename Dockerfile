FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"
ENV PIP_INSTALL="python3 -m pip install"

ADD https://deb.nodesource.com/setup_14.x /tmp
ADD https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb /tmp
ADD https://packages.microsoft.com/config/ubuntu/21.04/packages-microsoft-prod.deb /tmp
COPY dist/bzt*whl /tmp

WORKDIR /tmp
# add node repo and call 'apt-get update'
RUN bash ./setup_14.x && $APT_INSTALL build-essential python3-pip python3.10-dev net-tools apt-utils

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.10 1

# install python packages..
RUN $PIP_INSTALL ./bzt*whl chardet

# Fix vulnerabilities / outdated versions
RUN $PIP_INSTALL --user --upgrade pip pillow oauthlib pyjwt httplib2 numpy

RUN $APT_UPDATE && $APT_INSTALL \
    unzip software-properties-common apt-transport-https \
    openjdk-11-jdk xvfb siege apache2-utils git make nodejs locales tsung dotnet-sdk-6.0 libtool libssl-dev libyaml-dev libxml2-dev libxslt-dev

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
    && rbenv install 3.2.2 && rbenv global 3.2.2 && rbenv rehash \
    && gem install rspec rake selenium-webdriver cgi:0.3.5 && gem update bundler date && gem cleanup 
#    && rm /usr/local/rbenv/versions/3.2.2/lib/ruby/gems/3.2.0/specifications/default/cgi-0.3.6.gemspec \
#    && rm /usr/local/rbenv/versions/3.2.2/lib/ruby/gems/3.2.0/specifications/bundler-2.4.16.gemspec \
#    && rm /usr/local/rbenv/versions/3.2.2/lib/ruby/gems/3.2.0/specifications/default/date-3.3.3.gemspec

# firefox repo - do not use snap
RUN printf '%s\n' 'Package: firefox*' 'Pin: release o=Ubuntu*' 'Pin-Priority: -1' > /etc/apt/preferences.d/firefox-no-snap
RUN add-apt-repository ppa:mozillateam/ppa
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
SHELL ["/bin/sh", "-cl"]
RUN mkdir -p /etc/bzt.d \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && cp `python3 -c "import bzt; print('{}/resources/chrome_launcher.sh'.format(bzt.__path__[0]))"` \
    /opt/google/chrome/google-chrome \
  && bzt -install-tools -v \
  && google-chrome-stable --version && firefox --version && dotnet --version | head -1

### remove unused pem files
WORKDIR /root/.bzt/python-packages/3.10.6/gevent/tests
RUN rm -rf *.pem

# Fix npm vulnerabilites
WORKDIR /root/.bzt/selenium-taurus/wdio/node_modules/recursive-readdir
RUN sed -i 's/3.0.4/3.0.8/g' package.json && npm update && npm install -g npm@latest && npm -g update

WORKDIR /root/.bzt/newman/node_modules/tough-cookie
RUN sed -i 's/3.0.1/4.1.3/g' package.json

WORKDIR /root/.bzt/newman/node_modules/postman-request/node_modules/tough-cookie
RUN sed -i 's/2.5.0/4.1.3/g' package.json

RUN rm -rf /usr/share/javascript/jquery && rm -rf /usr/share/javascript/jquery-ui && rm -rf /tmp/* && mkdir /bzt-configs /tmp/artifacts

# Rootless user
# USER 1337:0
WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-cl", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

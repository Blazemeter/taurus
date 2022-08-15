FROM ubuntu:21.10

#replace 21.10 sources
RUN sed -i 's/archive.ubuntu.com/old-releases.ubuntu.com/' /etc/apt/sources.list
RUN sed -i 's/security.ubuntu.com/old-releases.ubuntu.com/' /etc/apt/sources.list

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"
ENV PIP_INSTALL="python3 -m pip install"

ADD https://deb.nodesource.com/setup_12.x /tmp
ADD https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb /tmp
ADD https://packages.microsoft.com/config/ubuntu/21.04/packages-microsoft-prod.deb /tmp
COPY dist/bzt*whl /tmp

WORKDIR /tmp
# add node repo and call 'apt-get update'
RUN bash ./setup_12.x && $APT_INSTALL build-essential python3-pip python3.9-dev

RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.9 1

# install python packages..
RUN $PIP_INSTALL ./bzt*whl chardet

# Fix vulnerabilities / outdated versions
RUN $PIP_INSTALL --user --upgrade pip pillow oauthlib pyjwt httplib2 numpy

RUN $APT_UPDATE && $APT_INSTALL \
    unzip software-properties-common apt-transport-https \
    openjdk-11-jdk xvfb siege apache2-utils firefox ruby ruby-dev make nodejs locales tsung

# set en_US.UTF-8 as default locale
RUN locale-gen "en_US.UTF-8" && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8

# Force cgi version to fix CVE-2021-41816
RUN gem install rspec rake selenium-webdriver cgi:0.1.1 && gem update bundler date && gem cleanup

# Get Google Chrome
RUN $APT_INSTALL ./google-chrome-stable_current_amd64.deb \
  && mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome

# Get .NET Core
RUN $APT_INSTALL ./packages-microsoft-prod.deb \
   # Update is required because packages-microsoft-prod.deb installation add repositories for dotnet
   && $APT_UPDATE \
   && $APT_INSTALL dotnet-sdk-3.1

# Install K6
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C5AD17C747E3415A3642D57D77C6C491D6AC1D69 \
   && echo "deb https://dl.k6.io/deb stable main" | tee /etc/apt/sources.list.d/k6.list \
   && $APT_UPDATE \
   && $APT_INSTALL k6

# Install Vegeta
ENV VEGETA_VERSION 12.8.4
RUN wget -q "https://github.com/tsenart/vegeta/releases/download/v${VEGETA_VERSION}/vegeta_${VEGETA_VERSION}_linux_amd64.tar.gz" -O /tmp/vegeta.tar.gz \
 && tar xzf /tmp/vegeta.tar.gz -C /bin \
 && rm /tmp/vegeta.tar.gz

# auto installable tools
RUN mkdir -p /etc/bzt.d \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && cp `python3 -c "import bzt; print('{}/resources/chrome_launcher.sh'.format(bzt.__path__[0]))"` \
    /opt/google/chrome/google-chrome \
  && bzt -install-tools -v \
  && google-chrome-stable --version && firefox --version && dotnet --version | head -1

# Fix npm vulnerabilites
WORKDIR /root/.bzt/selenium-taurus/wdio/node_modules/recursive-readdir
RUN sed -i 's/3.0.4/3.0.8/g' package.json && npm update && npm install -g npm@latest && npm -g update

RUN rm -rf /usr/share/javascript/jquery && rm -rf /usr/share/javascript/jquery-ui && rm -rf /tmp/* && mkdir /bzt-configs /tmp/artifacts

# Rootless user
# USER 1337:0
WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

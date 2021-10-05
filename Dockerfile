FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"
ENV PIP_INSTALL="python3 -m pip install"

ADD https://deb.nodesource.com/setup_12.x /tmp
ADD https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb /tmp
ADD https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb /tmp
COPY dist/bzt*whl /tmp

WORKDIR /tmp
# add node repo and call 'apt-get update'
RUN bash ./setup_12.x \
   && $APT_INSTALL \
     python3-pip unzip build-essential python3-dev software-properties-common \
     apt-transport-https openjdk-11-jdk xvfb siege tsung apache2-utils firefox ruby nodejs locales

# set en_US.UTF-8 as default locale
RUN locale-gen "en_US.UTF-8" \
   && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8

RUN $PIP_INSTALL setuptools wheel
RUN gem install rspec rake selenium-webdriver

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

# Install Taurus & tools
RUN $PIP_INSTALL ./bzt*whl \
  && mkdir -p /etc/bzt.d \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && cp `python3 -c "import bzt; print('{}/resources/chrome_launcher.sh'.format(bzt.__path__[0]))"` \
    /opt/google/chrome/google-chrome \
  && bzt -install-tools -v \
  && google-chrome-stable --version && firefox --version && dotnet --version | head -1

RUN rm -rf /tmp/* \
  && mkdir /bzt-configs /tmp/artifacts

WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

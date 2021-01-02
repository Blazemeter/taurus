FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"

ADD https://deb.nodesource.com/setup_12.x /tmp
ADD https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb /tmp
ADD https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb /tmp
COPY dist/bzt-*.tar.gz /tmp

WORKDIR /tmp
# add node repo and call 'apt-get update'
RUN bash ./setup_12.x \
   && $APT_INSTALL \
     python3-pip unzip build-essential python3-dev software-properties-common \
     apt-transport-https openjdk-8-jdk xvfb siege tsung apache2-utils firefox ruby nodejs

RUN python3 -m pip install setuptools wheel cython \
   && python3 -m pip install locust robotframework robotframework-seleniumlibrary molotov==1.6 twine
RUN gem install rspec rake selenium-webdriver

# Get Google Chrome
RUN $APT_INSTALL ./google-chrome-stable_current_amd64.deb \
  && mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome
COPY bzt/resources/chrome_launcher.sh /opt/google/chrome/google-chrome
RUN chmod +x /opt/google/chrome/google-chrome

# Get .NET Core
RUN apt-get install ./packages-microsoft-prod.deb \
   # Update is required because packages-microsoft-prod.deb installation add repositories for dotnet
   && $APT_UPDATE \
   && $APT_INSTALL dotnet-sdk-3.1

# Install Taurus & tools
RUN google-chrome-stable --version && firefox --version && dotnet --version | head -1 \
  && python3 -m pip install bzt-*.tar.gz \
  && mkdir -p /etc/bzt.d \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && bzt -install-tools -v

RUN rm -rf /tmp/* \
  && mkdir /bzt-configs /tmp/artifacts

WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

FROM ubuntu:16.04
RUN apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && add-apt-repository ppa:yandex-load/main \
  && apt-add-repository ppa:nilarimogard/webupd8 \
  && apt-get -y update \
  && apt-cache policy firefox \
  && apt-get -y install --no-install-recommends \
    kmod \
    unzip \
    build-essential \
    libxslt1-dev \
    zlib1g-dev \
    libxi6 \
    libgconf-2-4 \
    python-dev \
    python-pip \
    default-jdk \
    xvfb \
    libyaml-dev \
    siege \
    tsung \
    phantom \
    phantom-ssl \
    firefox=45.0.2+build1-0ubuntu1 \
    chromium-browser \
    pepperflashplugin-nonfree \
    flashplugin-installer \
    phantomjs \
    ruby ruby-dev \
    nodejs \
    npm \
  && pip install --upgrade setuptools pip \
  && pip install locustio bzt \
  && npm install -g mocha \
  && gem install rspec \
  && gem install selenium-webdriver \
  && firefox --version \
  && chromium-browser --version \
  && rm -rf /var/lib/apt/lists/*

ADD http://gettaurus.org/snapshots/blazemeter-pbench-extras_0.1.10.1_amd64.deb /tmp
ADD http://chromedriver.storage.googleapis.com/2.24/chromedriver_linux64.zip /tmp
RUN dpkg -i /tmp/blazemeter-pbench-extras_0.1.10.1_amd64.deb \
  && unzip -d /usr/bin /tmp/chromedriver_linux64.zip && /usr/bin/chromedriver --version

COPY . /tmp/bzt-src
RUN pip install --upgrade /tmp/bzt-src \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && echo '{"modules": {"console": {"disable": true}}}' > /etc/bzt.d/90-no-console.json

RUN cd /tmp/bzt-src/examples \
  && bzt /tmp/bzt-src/examples/all-executors.yml -o settings.artifacts-dir=/tmp/all-executors-artifacts || echo error

RUN cd /tmp/all-executors-artifacts \
  && ls -la \
  && grep \  * \
  && exit 1

RUN mkdir /bzt-configs \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /tmp/*

WORKDIR /bzt-configs
CMD bzt -l /tmp/artifacts/bzt.log /bzt-configs/*.yml

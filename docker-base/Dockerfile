FROM ubuntu:16.04

ADD https://deb.nodesource.com/setup_6.x /tmp
RUN apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && add-apt-repository ppa:yandex-load/main \
  && bash /tmp/setup_6.x \
  && apt-get -y update \
  && apt-get -y install --no-install-recommends \
    mc \
    tzdata \
    kmod \
    unzip \
    build-essential \
    libxslt1-dev \
    zlib1g-dev \
    libxi6 \
    libgconf-2-4 \
    libexif12 \
    udev \
    xvfb \
    python-dev \
    python-pip \
    default-jdk \
    libyaml-dev \
    ruby ruby-dev \
    nodejs \
    siege \
    tsung \
    apache2-utils \
    phantom \
    phantom-ssl \
    phantomjs \
  && apt-get -y install --no-install-recommends mono-complete nuget \
  && nuget update -self \
  && npm install -g mocha \
  && gem install rspec \
  && pip install --upgrade setuptools pip \
  && apt-get clean \
  && rm -rf /tmp/*

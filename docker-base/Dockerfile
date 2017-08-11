FROM ubuntu:16.04

RUN date && apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && apt-get -y update \
  && date \
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
  && date \
  && apt-get -y install --no-install-recommends mono-complete nuget \
  && date \
  && nuget update -self \
  && date \
  && pip install --upgrade setuptools pip \
  && apt-get clean

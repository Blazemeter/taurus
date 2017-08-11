FROM ubuntu:16.04

RUN date && apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && apt-get -y update \
  && date \
  && apt-get -y install --no-install-recommends mono-complete \
  && date \
  && apt-get -y install --no-install-recommends \
    kmod \
    unzip \
    build-essential \
    libxslt1-dev \
    zlib1g-dev \
    libxi6 \
    libgconf-2-4 \
    libexif12 \
    udev \
    python-dev \
    python-pip \
    default-jdk \
    xvfb \
    libyaml-dev \
    ruby ruby-dev \
    nodejs \
    nuget \
  && date \
  && nuget update -self \
  && pip install --upgrade setuptools pip \
  && apt-get clean

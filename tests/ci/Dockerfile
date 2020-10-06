FROM ubuntu:18.04

ENV DBUS_SESSION_BUS_ADDRESS=/dev/null DEBIAN_FRONTEND=noninteractive APT_INSTALL="apt-get -y install --no-install-recommends"

ADD https://dl-ssl.google.com/linux/linux_signing_key.pub /tmp
RUN apt-get -y update \
  && apt-get -y install dirmngr git \
  && $APT_INSTALL software-properties-common apt-utils \
  && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
  && cat /tmp/linux_signing_key.pub | apt-key add - \
  && apt-add-repository multiverse -y \
  && echo "deb http://download.mono-project.com/repo/ubuntu bionic main" | tee /etc/apt/sources.list.d/mono-official.list \
  && apt-add-repository ppa:yandex-load/main -y \
  && apt-add-repository ppa:nilarimogard/webupd8 -y \
  && $APT_INSTALL tzdata \
  && dpkg-reconfigure --frontend noninteractive tzdata \
  && $APT_INSTALL \
    language-pack-en mc kmod unzip build-essential \
    libxslt1-dev libffi-dev libxi6 libgconf-2-4 libexif12 libyaml-dev \
    udev openjdk-8-jdk xvfb siege tsung apache2-utils phantom phantom-ssl \
    pepperflashplugin-nonfree flashplugin-installer \
    mono-complete nuget net-tools gcc-mingw-w64-x86-64 \
  && $APT_INSTALL python3-dev python3-pip \
  && python3 -m pip install --upgrade pip \
  && python3 -m pip install --user --upgrade setuptools wheel \
  && nuget update -self \
  && apt-get clean

RUN apt-get update -y \
    && apt-get install -y \
    gcc-mingw-w64-x86-64 nsis libssl-dev libncurses5-dev libsqlite3-dev \
    libreadline-dev libtk8.5 libgdm-dev libdb4o-cil-dev libpcap-dev curl \
    && apt-get clean

# COPY . /artifacts
# WORKDIR /artifacts
# RUN ./build-sdist.sh
# RUN ./build-artifacts.sh
FROM ubuntu:18.04

ENV DBUS_SESSION_BUS_ADDRESS=/dev/null DEBIAN_FRONTEND=noninteractive APT_INSTALL="apt-get -y install --no-install-recommends"

WORKDIR /tmp
ADD https://dl-ssl.google.com/linux/linux_signing_key.pub /tmp
ADD https://deb.nodesource.com/setup_8.x /tmp
RUN apt-get -y update \
  && apt-get -y install dirmngr \
  && $APT_INSTALL software-properties-common apt-utils \
  && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
  && cat /tmp/linux_signing_key.pub | apt-key add - \
  && apt-add-repository multiverse \
  && echo "deb http://download.mono-project.com/repo/ubuntu bionic main" | tee /etc/apt/sources.list.d/mono-official.list \
  && apt-add-repository ppa:yandex-load/main \
  && apt-add-repository ppa:nilarimogard/webupd8 \
  && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list \
  && bash /tmp/setup_8.x \
  && $APT_INSTALL tzdata \
  && dpkg-reconfigure --frontend noninteractive tzdata \
  && $APT_INSTALL \
    language-pack-en mc kmod unzip build-essential \
    libxslt1-dev libffi-dev libxi6 libgconf-2-4 libexif12 libyaml-dev \
    udev openjdk-8-jdk xvfb siege tsung apache2-utils phantom phantom-ssl \
    firefox google-chrome-stable pepperflashplugin-nonfree flashplugin-installer \
    ruby ruby-dev nodejs mono-complete nuget net-tools gcc-mingw-w64-x86-64 \
  && $APT_INSTALL python3-dev python3-pip \
  && python3 -m pip install --upgrade pip \
  && python3 -m pip install --user --upgrade setuptools wheel \
  && python3 -m pip install robotframework robotframework-seleniumlibrary molotov twine \
  && gem install rspec rake \
  && gem install selenium-webdriver \
  && wget https://s3.amazonaws.com/deployment.blazemeter.com/jobs/taurus-pbench/10/blazemeter-pbench-extras_0.1.10.1_amd64.deb \
  && dpkg -i /tmp/blazemeter-pbench-extras_0.1.10.1_amd64.deb \
  && nuget update -self \
  && apt-get clean

COPY bzt/resources/chrome_launcher.sh /tmp
RUN mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome \
  && mv /tmp/chrome_launcher.sh /opt/google/chrome/google-chrome \
  && chmod +x /opt/google/chrome/google-chrome

COPY . /tmp/bzt-src
WORKDIR /tmp/bzt-src
RUN google-chrome-stable --version && firefox --version && mono --version && nuget | head -1 \
  && ./build-sdist.sh \
  && python3 -m pip install dist/bzt-*.tar.gz \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && bzt -install-tools -v && ls -la /tmp && cat /tmp/jpgc-*.log && ls -la ~/.bzt/jmeter-taurus/*/lib/ext && ls -la ~/.bzt/jmeter-taurus/*/lib/ext/jmeter-plugins-tst-*.jar

RUN mkdir /bzt-configs \
  && rm -rf /tmp/* \
  && mkdir /tmp/artifacts

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8
WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

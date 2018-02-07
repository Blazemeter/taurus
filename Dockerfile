FROM ubuntu:16.04

ENV DBUS_SESSION_BUS_ADDRESS=/dev/null

WORKDIR /tmp
ADD https://dl-ssl.google.com/linux/linux_signing_key.pub /tmp
ADD https://deb.nodesource.com/setup_6.x /tmp
RUN apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
  && echo "deb http://download.mono-project.com/repo/ubuntu xenial main" | tee /etc/apt/sources.list.d/mono-official.list \
  && add-apt-repository ppa:yandex-load/main \
  && apt-add-repository ppa:nilarimogard/webupd8 \
  && cat /tmp/linux_signing_key.pub | apt-key add - \
  && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list \
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
    python-dev \
    python-pip \
    default-jdk \
    xvfb \
    libyaml-dev \
    siege \
    tsung \
    apache2-utils \
    phantom \
    phantom-ssl \
    firefox \
    google-chrome-stable \
    pepperflashplugin-nonfree \
    flashplugin-installer \
    phantomjs \
    ruby ruby-dev \
    nodejs \
    mono-complete nuget \
    python3-dev python3-pip \
    net-tools \
    autoconf \
  && pip install --upgrade setuptools pip \
  && pip install locustio bzt && pip uninstall -y bzt \
  && pip install robotframework robotframework-seleniumlibrary \
  && pip3 install --upgrade setuptools wheel \
  && pip3 install molotov \
  && npm install -g mocha \
  && gem install rspec \
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
  && pip install dist/bzt-*.tar.gz \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && bzt -install-tools -v && ls -la /tmp && cat /tmp/jpgc-*.log && ls -la ~/.bzt/jmeter-taurus/*/lib/ext && ls -la ~/.bzt/jmeter-taurus/*/lib/ext/jmeter-plugins-tst-*.jar

RUN bzt /tmp/bzt-src/examples/all-executors.yml -o settings.artifacts-dir=/tmp/all-executors-artifacts -sequential || (\
  ls -lh /tmp/all-executors-artifacts; \
  cat /tmp/all-executors-artifacts/nose-0.err; \
  cat /tmp/all-executors-artifacts/geckodriver.log; \
  cat /tmp/all-executors-artifacts/processlist.txt; \
  exit 1)

RUN mkdir /bzt-configs \
  && rm -rf /tmp/* \
  && mkdir /tmp/artifacts

WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

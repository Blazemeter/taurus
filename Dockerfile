FROM ubuntu:16.04

ENV DBUS_SESSION_BUS_ADDRESS=/dev/null

ADD https://s3.amazonaws.com/deployment.blazemeter.com/jobs/taurus-pbench/10/blazemeter-pbench-extras_0.1.10.1_amd64.deb /tmp
ADD https://dl-ssl.google.com/linux/linux_signing_key.pub /tmp
ADD https://deb.nodesource.com/setup_6.x /tmp
RUN date && apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && add-apt-repository ppa:yandex-load/main \
  && apt-add-repository ppa:nilarimogard/webupd8 \
  && cat /tmp/linux_signing_key.pub | apt-key add - \
  && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list \
  && bash /tmp/setup_6.x \
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
    nuget \
  && date \
  && pip install --upgrade setuptools pip \
  && pip install locustio bzt && pip uninstall -y bzt \
  && pip install --upgrade selenium \
  && npm install -g mocha \
  && gem install rspec \
  && gem install selenium-webdriver \
  && dpkg -i /tmp/blazemeter-pbench-extras_0.1.10.1_amd64.deb \
  && nuget update -self \
  && apt-get clean \
  && firefox --version && google-chrome-stable --version && mono --version && nuget | head -1

COPY bzt/resources/chrome_launcher.sh /tmp
RUN mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome \
  && mv /tmp/chrome_launcher.sh /opt/google/chrome/google-chrome \
  && chmod +x /opt/google/chrome/google-chrome

COPY . /tmp/bzt-src
WORKDIR /tmp/bzt-src
RUN ./build-sdist.sh \
  && pip install dist/bzt-*.tar.gz \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json

RUN bzt -install-tools -v && bzt /tmp/bzt-src/examples/all-executors.yml -o settings.artifacts-dir=/tmp/all-executors-artifacts -sequential || (ls -lh /tmp/all-executors-artifacts ; exit 1)

RUN mkdir /bzt-configs \
  && rm -rf /tmp/* \
  && mkdir /tmp/artifacts

WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

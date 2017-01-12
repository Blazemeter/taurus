FROM ubuntu:16.04
ADD http://gettaurus.org/snapshots/blazemeter-pbench-extras_0.1.10.1_amd64.deb /tmp
ADD http://chromedriver.storage.googleapis.com/2.25/chromedriver_linux64.zip /tmp
ADD https://github.com/mozilla/geckodriver/releases/download/v0.11.1/geckodriver-v0.11.1-linux64.tar.gz /tmp
ADD https://dl-ssl.google.com/linux/linux_signing_key.pub /tmp
ADD https://deb.nodesource.com/setup_6.x /tmp
RUN apt-get -y update \
  && apt-get -y install --no-install-recommends software-properties-common \
  && apt-add-repository multiverse \
  && add-apt-repository ppa:yandex-load/main \
  && apt-add-repository ppa:nilarimogard/webupd8 \
  && cat /tmp/linux_signing_key.pub | apt-key add - \
  && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list \
  && bash /tmp/setup_6.x \
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
  && pip install --upgrade setuptools pip \
  && pip install locustio bzt && pip uninstall -y bzt \
  && pip install --upgrade selenium \
  && npm install -g mocha \
  && gem install rspec \
  && gem install selenium-webdriver \
  && dpkg -i /tmp/blazemeter-pbench-extras_0.1.10.1_amd64.deb \
  && unzip -d /usr/bin /tmp/chromedriver_linux64.zip \
  && tar -xzf /tmp/geckodriver-v0.11.1-linux64.tar.gz --directory /usr/local/bin \
  && rm -rf /var/lib/apt/lists/* \
  && firefox --version && google-chrome-stable --version && /usr/bin/chromedriver --version && geckodriver --version

COPY bzt/resources/chrome_launcher.sh /tmp
RUN mv /opt/google/chrome/google-chrome /opt/google/chrome/_google-chrome \
  && mv /tmp/chrome_launcher.sh /opt/google/chrome/google-chrome \
  && chmod +x /opt/google/chrome/google-chrome
ENV DBUS_SESSION_BUS_ADDRESS=/dev/null

COPY . /tmp/bzt-src
RUN pip install /tmp/bzt-src \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && echo '{"modules": {"console": {"disable": true}}}' > /etc/bzt.d/90-no-console.json

RUN bzt /tmp/bzt-src/examples/all-executors.yml -o settings.artifacts-dir=/tmp/all-executors-artifacts -sequential || (cat /tmp/all-executors-artifacts/webdriver-1.log; exit 1)

RUN mkdir /bzt-configs \
  && rm -rf /tmp/* \
  && mkdir /tmp/artifacts

WORKDIR /bzt-configs
ENTRYPOINT ["sh", "-c", "bzt -l /tmp/artifacts/bzt.log \"$@\"", "ignored"]

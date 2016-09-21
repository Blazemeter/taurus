FROM ubuntu:14.04
RUN apt-get -y update \
  && apt-get -y --force-yes install --no-install-recommends \
    gcc \
    libxslt1-dev \
    zlib1g-dev \
    python-dev \
    python-pip \
    default-jre-headless \
    xvfb \
    libyaml-dev \
    siege \
  && pip install bzt==1.6.5 \
  && echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json \
  && echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json \
  && echo '{"modules": {"console": {"disable": true}}}' > /etc/bzt.d/90-no-console.json \
  && bzt -o settings.default-executor=jmeter \
    -o execution.scenario.requests.0=http://localhost/ \
    -o execution.iterations=1 -o execution.hold-for=1 \
    -o execution.throughput=1 \
  && mkdir /bzt-configs \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /bzt-configs
CMD bzt -l /tmp/artifacts/bzt.log /bzt-configs/*.yml

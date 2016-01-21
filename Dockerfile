FROM ubuntu:14.04
RUN apt-get -y update
RUN apt-get -y --force-yes install libxslt1-dev
RUN apt-get -y --force-yes install zlib1g-dev
RUN apt-get -y --force-yes install python-dev
RUN apt-get -y --force-yes install python-pip
RUN apt-get -y --force-yes install default-jre-headless
RUN apt-get -y --force-yes install xvfb
RUN apt-get -y --force-yes install libyaml-dev
RUN apt-get -y --force-yes install siege
RUN pip install bzt
RUN echo '{"install-id": "Docker"}' > /etc/bzt.d/99-zinstallID.json
RUN echo '{"settings": {"artifacts-dir": "/tmp/artifacts"}}' > /etc/bzt.d/90-artifacts-dir.json
RUN echo '{"modules": {"console": {"disable": true}}}' > /etc/bzt.d/90-no-console.json
RUN bzt -o settings.default-executor=jmeter -o execution.scenario.requests.0=http://localhost/ -o execution.iterations=1 -o execution.hold-for=1 -o execution.throughput=1
RUN mkdir /bzt-configs
CMD cd /bzt-configs  && bzt -l /tmp/artifacts/bzt.log /bzt-configs/*.yml

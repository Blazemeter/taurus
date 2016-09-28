# 1: Start off from the OpenJDK JRE Alpine image:
FROM openjdk:jre-alpine

# 2: Install runtime dependencies:
RUN set -ex \
  && apk add --no-cache --virtual .app-rundeps python xvfb py-pip libxslt zlib libxml2

# 3: Install build dependencies and build/install bzt:
RUN set -ex \
  && export BZT_VERSION=1.6.8 \
  && apk add --no-cache --virtual .app-builddeps build-base linux-headers python-dev libxslt-dev zlib-dev libxml2-dev \
  && pip install bzt==${BZT_VERSION} \
  && mkdir -p /tmp/artifacts \
  && bzt -o settings.default-executor=jmeter \
    -o execution.scenario.requests.0=http://localhost/ \
    -o execution.iterations=1 -o execution.hold-for=1 \
    -o execution.throughput=1 \
    -o settings.artifacts-dir=/tmp/artifacts \
    -o modules.console.disable=true \
    -l /tmp/artifacts/bzt.log \
  && mkdir /bzt-configs \
  && apk del .app-builddeps \
  && rm -rf /tmp/*

# 4: Set the working directory:
WORKDIR /bzt-configs

# 5: Set the default command:
CMD bzt -l /tmp/artifacts/bzt.log -o settings.artifacts-dir=/tmp/artifacts /bzt-configs/*.yml

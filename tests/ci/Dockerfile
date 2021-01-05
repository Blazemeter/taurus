# Notes:
# don't worry about apt-utils warning - just fake
# it works correctly with cache
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"

RUN $APT_UPDATE \
   && $APT_INSTALL unzip gnupg curl python3-pip software-properties-common apt-transport-https git gcc-mingw-w64-x86-64

# following block should be optimized
RUN echo "deb http://packages.cloud.google.com/apt cloud-sdk main" >> /etc/apt/sources.list.d/google-cloud-sdk.list \
 && echo "deb https://apt.kubernetes.io/ kubernetes-xenial main" >> /etc/apt/sources.list.d/kubernetes.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - \
 && add-apt-repository -n "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
RUN $APT_UPDATE && $APT_INSTALL docker-ce-cli kubectl google-cloud-sdk

# at the moment setuptools is included to python3-pip
RUN python3 -m pip install setuptools twine pyyaml

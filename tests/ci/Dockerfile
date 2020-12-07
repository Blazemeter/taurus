# Notes:
# don't worry about apt-utils warning - just fake

FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
#
RUN apt-get -y update \
   && $APT_INSTALL dirmngr gnupg python3-pip software-properties-common gcc-mingw-w64-x86-64 curl build-essential \
   zlib1g-dev apt-transport-https ca-certificates \
   libffi-dev libgconf-2-4 libexif12 \
   libyaml-dev net-tools python3-dev nsis libssl-dev libncurses5-dev

# following block should be optimized
RUN echo "deb http://packages.cloud.google.com/apt cloud-sdk main" >> /etc/apt/sources.list.d/google-cloud-sdk.list \
 && echo "deb https://apt.kubernetes.io/ kubernetes-xenial main" >> /etc/apt/sources.list.d/kubernetes.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - \
 && add-apt-repository -n "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
RUN apt-get update && $APT_INSTALL docker-ce-cli containerd.io kubectl google-cloud-sdk

RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# at the moment setuptools is included to python3-pip
RUN python3 -m pip install setuptools twine pythonz-bd virtualenv
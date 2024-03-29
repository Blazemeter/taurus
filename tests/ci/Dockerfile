# Notes:
# don't worry about apt-utils warning - just fake
# it works correctly with cache
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1
ENV APT_INSTALL="apt-get -y install --no-install-recommends"
ENV APT_UPDATE="apt-get -y update"

RUN $APT_UPDATE \
   && $APT_INSTALL unzip gnupg curl python3-pip software-properties-common ca-certificates apt-transport-https git gcc-mingw-w64-x86-64

# following block should be optimized
RUN curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && echo "deb https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list  \
 && curl -fsSL https://pkgs.k8s.io/core:/stable:/v1.28/deb/Release.key | gpg --dearmor -o /etc/apt/keyrings/kubernetes-apt-keyring.gpg \
 && echo "deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] https://pkgs.k8s.io/core:/stable:/v1.28/deb/ /" | tee /etc/apt/sources.list.d/kubernetes.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker-archive-keyring.gpg \
 && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null
RUN $APT_UPDATE && $APT_INSTALL docker-ce-cli kubectl

# Install GCE CLI
RUN curl -sSL https://sdk.cloud.google.com | bash
ENV PATH $PATH:/root/google-cloud-sdk/bin

# Install gke gcloud auth plugin
RUN /root/google-cloud-sdk/bin/gcloud components install gke-gcloud-auth-plugin
RUN $APT_INSTALL google-cloud-sdk-gke-gcloud-auth-plugin

# at the moment setuptools is included to python3-pip
RUN python3 -m pip install setuptools twine pyyaml

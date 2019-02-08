FROM docker:18.09

RUN apk add --update python py-pip curl which bash zip
RUN pip install pyyaml
RUN curl -sSL https://sdk.cloud.google.com | bash

ENV PATH $PATH:/root/google-cloud-sdk/bin

RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl
RUN chmod +x ./kubectl && mv ./kubectl /usr/local/bin/kubectl

RUN mkdir /bzt
WORKDIR /bzt

ENTRYPOINT ["sh", "-c"]
#! /bin/bash -xe
#sudo docker build . -t bztrelease
sudo docker run --rm -v `cd $(dirname $0) && pwd`:/release -v ~:/homedir --entrypoint /bin/bash -t bztrelease -c \
  "apt-get update && apt-get -y install maven && cp -r /release /tmp/release && cp /homedir/.pypirc /root/ && cd /tmp/release && ./release.bash"
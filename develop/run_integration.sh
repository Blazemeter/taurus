#!/bin/zsh
#This script is meant for internal testing
#If you're making changes to the library versions, you need to refresh base image (it's sufficient to delete it)
cd ..
docker image rm local_taurus_develop -f
docker pull us.gcr.io/verdant-bulwark-278/jenkins-docker-agent:taurus-agent-2
docker run -v $(pwd):/tmp/taurus -w /tmp/taurus us.gcr.io/verdant-bulwark-278/jenkins-docker-agent:taurus-agent-2 sh -c "./build-artifacts.sh"
[ -n "$(docker images -q local_taurus_base)" ] || docker build -f develop/Dockerfile-base -t local_taurus_base .
docker build -f develop/Dockerfile-taurus --no-cache -t local_taurus_develop .
docker run -v $(pwd):/bzt-configs -v $(pwd)/integr-artifacts:/tmp/artifacts local_taurus_develop -sequential examples/all-executors.yml
cd develop
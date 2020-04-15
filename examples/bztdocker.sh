sudo rm -rf `pwd`/docker-artifacts
docker run -it --rm -v `pwd`:/bzt-configs -v `pwd`/docker-artifacts:/tmp/artifacts blazemeter/taurus "$@"
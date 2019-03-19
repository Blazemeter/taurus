#!/bin/bash
sudo docker build -t bztm . && sudo docker run -it -v `pwd`:/tmp bztm /tmp/test.yml -report
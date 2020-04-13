#!/bin/bash
docker build -t bztm . && docker run -it -v `pwd`:/tmp bztm /tmp/test.yml -report
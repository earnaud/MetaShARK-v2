#!/bin/sh

docker stop MetaShARK_dockerized 
docker build -t metashark:local .
docker run -d --rm -v ~/dataPackagesOutput:/root/dataPackagesOutput -p 3838:3838 \
  --name MS metashark:local


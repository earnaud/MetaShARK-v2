#!/bin/sh

docker stop MetaShARK_dockerized 
docker build -t metashark:local .
docker run -d --rm   -v ~/metashark_data:/root/dataPackagesOutput   -p 3838:3838   --name MetaShARK_dockerized   metashark:local


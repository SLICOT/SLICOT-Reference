#!/usr/bin/env bash

export CI_REGISTRY_IMAGE=${CI_REGISTRY_IMAGE:-gitlab.mpi-magdeburg.mpg.de/software/slicot}
export IMAGE_NAME=${IMAGE_NAME:-fedora}
IMAGE_BASE=${CI_REGISTRY_IMAGE}/${IMAGE_NAME}


for i in Dockerfile.*
do
    echo $i
    v=${i#Dockerfile.fc}
    echo "Build $v"
    docker build -f "$i" -t ${IMAGE_BASE}:${v} .
    docker push ${IMAGE_BASE}:${v}
done


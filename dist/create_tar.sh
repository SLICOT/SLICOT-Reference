#!/usr/bin/env sh

VERSION=$(bash ./dist/get_version.sh)
if [ $# -eq 1 ]; then
    GIT=$1
else
    GIT=v${VERSION}
fi
git archive --format tar.gz --prefix slicot-${VERSION}/ -o slicot-${VERSION}.tar.gz -v $GIT

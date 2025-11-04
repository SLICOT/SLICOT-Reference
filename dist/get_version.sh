#!/bin/sh
VERSION=""
if [ -e ./.version ]; then
    . ./.version
    VERSION="$MAJOR.$MINOR.$PATCH"
fi

if [ -d .git -a -n "$(which git)" ]; then
    VERSIONX=$(git describe --match 'v[0-9]*.[0-9]*.[0-9]*' --long 2>/dev/null )
    ret=$?
    if [ $ret -eq 0 ]; then
        VERSION=$(echo $VERSIONX | sed 's/-g[0-9a-f]\+$//' | sed 's/^v//g' )
    fi
fi

if [ "$1" = "deb" ]; then
    VERSION=$(echo $VERSION | sed 's/-/+/g' )
fi
echo $VERSION


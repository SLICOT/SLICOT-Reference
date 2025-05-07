#!/bin/sh
# Build the RPM files
#
# This script must be executed from the root of the source.
#
#
. ./.version

BUILDARG=${1:--ta}

RPMSPEC_BASE=slicot.spec
RPMSPEC=dist/rpm/${RPMSPEC_BASE}.in

# MAJOR=$(cat "$RPMSPEC" | grep "%global major" | sed -e 's/%global major //g' | tr -d ' ' )
# MINOR=$(cat "$RPMSPEC" | grep "%global minor" | sed -e 's/%global minor //g' | tr -d ' ' )
# PATCH=$(cat "$RPMSPEC" | grep "%global patch" | sed -e 's/%global patch //g' | tr -d ' ' )
# cp "${RPMSPEC}" "${RPMSPEC_BASE}"

sed -e "s/##MAJOR##/$MAJOR/g" -e "s/##MINOR##/$MINOR/g" -e "s/##PATCH##/$PATCH/g" "${RPMSPEC}" > "${RPMSPEC_BASE}"
tar -v --exclude="build*" --exclude-vcs --exclude-vcs-ignores \
    --transform="s/.\/\(.*\)/slicot-${MAJOR}.${MINOR}.${PATCH}\/\1/g" \
    -czf /tmp/slicot-${MAJOR}.${MINOR}.${PATCH}.tar.gz .
rm "${RPMSPEC_BASE}"

rpmbuild ${BUILDARG} /tmp/slicot-${MAJOR}.${MINOR}.${PATCH}.tar.gz


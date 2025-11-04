#!/bin/bash
if [ $# -ne 1 ]; then
    echo "Usage: $0 DOI"
    exit 1
fi

export DOI=$1
sed -i -e "s,^doi:.*\$,doi: ${DOI},g" CITATION.cff



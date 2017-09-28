#!/bin/bash

# This script can be used on a computer where Guix is available, but
# where GuixSD is not used.

set -e
set -u

# This is needed to suppress locale warnings.  The "glibc-locales"
# package should be installed to this profile.
GUIX_PROFILE=/gnu/var/guix/profiles/custom/rcas/.guix-profile
export GUIX_LOCPATH=${GUIX_PROFILE}/lib/locale

# Add pandoc to PATH
export PATH=${GUIX_PROFILE}/bin:$PATH

# This must match the installation prefix.
ROOT=/srv/rcas-web

# Spawn web processes
for port in $(seq 3001 3020); do
    ${GUIX_PROFILE}/bin/rcas-web --config=${ROOT}/rcas.conf server ${port} \
             > ${ROOT}/logs/rcas-web-${port}.log &
    echo $! > ${ROOT}/pids/rcas-web-${port}.pid
done

# Spawn background workers
for id in $(seq 20); do
    ${GUIX_PROFILE}/bin/rcas-web --config=${ROOT}/rcas.conf worker \
             > ${ROOT}/logs/rcas-web-worker-${id}.log &
    echo $! > ${ROOT}/pids/rcas-web-worker-${id}.pid
done

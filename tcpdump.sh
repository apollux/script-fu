#!/bin/sh

# USAGE: ./tcpdump.sh <local-port> <host> <port>

# For the original netcat, add '-p' before ${local} in the netcat
# invocation.

# Reference: http://blog.hawkhost.com/2009/12/12/using-netcat-as-an-intercepting-proxy/

set -e

[ "x$1" = "x" ] && local=8080 || local=$1
[ "x$2" = "x" ] && host=www3.imperial.ac.uk || host=$2
[ "x$3" = "x" ] && port=80 || port=$3

nc=$(type nc 2> /dev/null && echo nc || echo netcat)
echo "Netcat is '${nc}'"

rm -f pipe
mkfifo pipe
echo "Forwarding localhost:${local} to ${host}:${port}"
{ ${nc} -l -p ${local} || ${nc} -l ${local}; } < pipe | tee outgoing.dump | ${nc} ${host} ${port} | tee pipe incoming.dump > /dev/null
rm -f pipe

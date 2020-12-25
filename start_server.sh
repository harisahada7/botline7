#!/bin/bash

# Script used to run chat bot server as linux service
# Need to create a service file for systemd that calls this script

make clean
eval $(opam config env)
make server 

exit 0

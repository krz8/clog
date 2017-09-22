#!/bin/sh
#
# This exists as a wrapper around the CCL Lisp image under most
# Unix-like systems.  The image has been dumped with its kernel intact
# and the compiled CLOG system present. However, when CCL starts up in
# any form, it still tries to process its command line.  This means
# that certain options still have an effect, like -b and others.
#
# This wrapper ensures that all command line arguments are passed to
# CLOG:MAIN via CCL:*COMMAND-LINE-ARGUMENT-LIST*.

exec clogk -n -- "$@"

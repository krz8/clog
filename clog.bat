REM  This exists as a wrapper around the CCL Lisp image under Windows.
REM  The image has been dumped with its kernel intact and the compiled CLOG
REM  system present. However, when CCL starts up in any form, it still
REM  tries to process its command line.  This means that certain options
REM  still have an effect, like -b and others.
REM
REM  This wrapper ensures that all command line arguments are passed to
REM  CLOG:MAIN via CCL:*COMMAND-LINE-ARGUMENT-LIST*.  %* will preserve most
REM  quoting operations at the CMD command line.

@echo off
clogk -n -- %*

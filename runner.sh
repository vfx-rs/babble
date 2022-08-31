#!/usr/bin/env bash

if [[ -z $DEBUG ]]; then
    exec $*
else
    exec lldb $*
fi
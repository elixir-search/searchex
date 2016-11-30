#!/bin/bash
MIX_ENV=prod mix escript.build
if [ $? -eq 0 ]; then
    echo OK
else
    echo FAIL
fi

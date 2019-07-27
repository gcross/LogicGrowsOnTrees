#!/bin/sh
stack build `cat examples/targets.lst` `cat tests/targets.lst`

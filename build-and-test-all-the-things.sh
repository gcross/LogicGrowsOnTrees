#!/bin/sh
stack build `cat tests/targets.lst` `cat examples/targets.lst` `cat tutorial/targets.lst`

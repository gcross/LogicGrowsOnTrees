#!/bin/sh
stack build --flag LogicGrowsOnTrees-processes:examples `cat tests/targets.lst` `cat examples/targets.lst` `cat benchmarks/targets.lst`

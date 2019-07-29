#!/bin/sh
stack build --flag LogicGrowsOnTrees:examples --flag LogicGrowsOnTrees:tutorials `cat tests/targets.lst` `cat examples/targets.lst` `cat tutorial/targets.lst` `cat benchmarks/targets.lst`

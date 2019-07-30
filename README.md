# exoerl

Erlang implementation of the EXOline-TCP protocol for Regin Corrigo heating/ventilation controllers.

Prototyping ground for improvements to https://github.com/merbanan/EXOlink

Unofficial and reverse-engineered, based on understanding gained by EXOlink. Some protocol aspects are not known. 

**Use this at your own risk. No warranties given.**

## Usage example
```
$ erl
c(exoerl).
exoerl:go("<ip-address>",outdoor).
exoerl:go("<ip-address>",[outdoor,radiator,hot_water,district_heating]).
exoerl:go("<ip-address>",outdoor).
exoerl:go("<ip-address>",<<16#02, 16#0c>>). % id of outdoor (subject to change as understanding improves)
exoerl:go("<ip-address>",12). % same as above 
```

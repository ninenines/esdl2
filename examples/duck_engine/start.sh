#!/bin/sh
erl -smp enable +stbt db -pa ../../ebin -eval "duck_engine:run()."

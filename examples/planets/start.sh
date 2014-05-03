#!/bin/sh
erl +stbt db -pa ../../ebin -eval "planets:run()."

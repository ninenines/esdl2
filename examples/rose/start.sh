#!/bin/sh
erl +stbt db -pa ../../ebin -eval "rose:run()."

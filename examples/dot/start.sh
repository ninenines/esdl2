#!/bin/sh
erl +stbt db -pa ../../ebin -eval "dot:run()."

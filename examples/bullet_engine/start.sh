#!/bin/sh
erl +stbt db -pa ../../ebin -eval "bullet_engine:run()."

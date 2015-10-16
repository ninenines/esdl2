#!/bin/sh
erl -smp enable +stbt db -pa ../../ebin -eval "bullet_engine:run()."

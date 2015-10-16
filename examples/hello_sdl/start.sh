#!/bin/sh
erl -smp enable +stbt db -pa ../../ebin -eval "hello_sdl:run()."

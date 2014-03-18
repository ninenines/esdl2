#!/bin/sh
erl +stbt db -pa ../../ebin -eval "hello_sdl:run()."

# Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

PROJECT = esdl2

# SDL 2.0.3 has this option enabled that causes problems with NIF functions.
SDL2_LIBS_FILTER_OUT = -Wl,--no-undefined
SDL2_LIBS = $(filter-out $(SDL2_LIBS_FILTER_OUT),$(shell sdl2-config --static-libs))

ifeq ($(OS),Windows_NT)
	CC_OPTS = ""
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		CC_OPTS = -undefined dynamic_lookup
    	endif
endif


C_SRC_OPTS = $(shell sdl2-config --cflags) $(SDL2_LIBS) -lSDL2_image  $(CC_OPTS)

include erlang.mk

bullet_engine:: all
	erlc -o examples/bullet_engine examples/bullet_engine/*.erl
	cd examples/bullet_engine && ./start.sh

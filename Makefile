# Copyright (c) 2014-2015, Loïc Hoguin <essen@ninenines.eu>
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
PROJECT_DESCRIPTION = SDL2 Erlang NIF.
PROJECT_VERSION = 0.1.0

# SDL 2.0.3 has this option enabled that causes problems with NIF functions.
SDL2_LIBS_FILTER_OUT = -Wl,--no-undefined
SDL2_LIBS = $(filter-out $(SDL2_LIBS_FILTER_OUT),$(shell sdl2-config --static-libs))

CFLAGS += $(shell sdl2-config --cflags)
# @todo -undefined dynamic_lookup on OSX?
LDLIBS += $(SDL2_LIBS) -lSDL2_image -lSDL2_ttf

include erlang.mk

ifeq ($(PLATFORM),msys2)
	CFLAGS += -I"$(C_SRC_DIR)/compat/"
endif

bullet_engine:: all
	erlc -o examples/bullet_engine examples/bullet_engine/*.erl
	cd examples/bullet_engine && ./start.sh

hello_sdl:: all
	erlc -o examples/hello_sdl examples/hello_sdl/*.erl
	cd examples/hello_sdl && ./start.sh

hello_sdl_text:: all
	erlc -o examples/hello_sdl_text examples/hello_sdl_text/*.erl
	cd examples/hello_sdl_text && ./start.sh

clean::
	@rm -f *.swp
	@rm -f c_src/env.mk
	@rm -rf ebin/
	@rm -f examples/*/*.beam
	@echo clean

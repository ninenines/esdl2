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

PRIV_DIR ?= ../priv

# SDL 2.0.3 has this option enabled that causes problems with NIF functions.
SDL2_LIBS_FILTER_OUT = -Wl,--no-undefined
SDL2_LIBS = $(filter-out $(SDL2_LIBS_FILTER_OUT),$(shell sdl2-config --static-libs))

all: env.mk
	mkdir -p $(PRIV_DIR)
	gcc *.c -fPIC -shared -o $(PRIV_DIR)/esdl2.so -I $(ERTS_INCLUDE_DIR) \
		`sdl2-config --cflags` $(SDL2_LIBS) -lSDL2_image

env.mk:
	erl -noshell -noinput -eval "file:write_file(\"env.mk\", \
		io_lib:format(\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/\", \
			[code:root_dir(), erlang:system_info(version)])), \
		init:stop()."

-include env.mk

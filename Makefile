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

all:
	rm -rf ebin/
	mkdir -p ebin/
	erlc -o ebin/ src/*.erl
	cd c_src && make
	erlc -o examples/bullet_engine examples/bullet_engine/*.erl
	cd examples/bullet_engine && ./start.sh

hello:
	erlc -o examples/hello_sdl examples/hello_sdl/*.erl
	cd examples/hello_sdl && ./start.sh

clean:
	@rm -f *.swp
	@rm -f c_src/env.mk
	@rm -rf ebin/
	@rm -f examples/*/*.beam
	@echo clean

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
	make ebin
	make bullet

ebin:
	rm -rf ebin/
	mkdir -p ebin/
	erlc -o ebin/ src/*.erl
	cd c_src && make

bullet: ebin
	erlc -o examples/bullet_engine examples/bullet_engine/*.erl
	cd examples/bullet_engine && ./start.sh

hello:  ebin
	erlc -o examples/hello_sdl examples/hello_sdl/*.erl
	cd examples/hello_sdl && ./start.sh

dot:    ebin
	erlc -o examples/dot examples/dot/*.erl
	cd examples/dot && ./start.sh

rose:   ebin
	erlc -o examples/rose examples/rose/*.erl
	cd examples/rose && ./start.sh

clean:
	@rm -f *.swp
	@rm -f c_src/env.mk
	@rm -f priv/esdl2.so
	@rm -rf ebin/
	@rm -f examples/*/*.dump
	@rm -f examples/*/*.beam
	@rm -f .DS_Store
	@echo clean

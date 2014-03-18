// Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#include "esdl2.h"
#include "SDL_image.h"

void dtor_Surface(ErlNifEnv* env, void* obj)
{
	SDL_FreeSurface(NIF_RES_GET(Surface, obj));
}

NIF_FUNCTION(img_load)
{
	char filename[FILENAME_MAX];
	SDL_Surface* surface;
	ERL_NIF_TERM term;

	BADARG_IF(!enif_get_string(env, argv[0], filename, FILENAME_MAX, ERL_NIF_LATIN1));

	surface = IMG_Load(filename);
	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_TERM(Surface, surface, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

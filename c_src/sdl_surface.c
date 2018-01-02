// Copyright (c) 2014-2018, Loïc Hoguin <essen@ninenines.eu>
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

// img_load

NIF_CALL_HANDLER(thread_img_load)
{
	SDL_Surface* surface;
	ERL_NIF_TERM term;

	surface = IMG_Load(args[0]);

	enif_free(args[0]);

	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_TERM(Surface, surface, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(img_load)
{
	unsigned int len;
	char *filename;

	BADARG_IF(!enif_get_list_length(env, argv[0], &len));
	filename = (char*)enif_alloc(len + 1);

	if (!enif_get_string(env, argv[0], filename, len + 1, ERL_NIF_LATIN1)) {
		enif_free(filename);
		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_img_load, 1, filename);
}

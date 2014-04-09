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

// get_base_path

NIF_FUNCTION(get_base_path)
{
	char* path;
	ERL_NIF_TERM term;

	path = SDL_GetBasePath();

	if (!path)
		return sdl_error_tuple(env);

	term = enif_make_string(env, path, ERL_NIF_LATIN1);

	SDL_free(path);

	return enif_make_tuple2(env, atom_ok, term);
}

// get_pref_path

NIF_FUNCTION(get_pref_path)
{
	char org[255], app[255];
	char* path;
	ERL_NIF_TERM term;

	BADARG_IF(!enif_get_string(env, argv[0], org, 255, ERL_NIF_LATIN1));
	BADARG_IF(!enif_get_string(env, argv[1], app, 255, ERL_NIF_LATIN1));

	path = SDL_GetPrefPath(org, app);

	if (!path)
		return sdl_error_tuple(env);

	term = enif_make_string(env, path, ERL_NIF_LATIN1);

	SDL_free(path);

	return enif_make_tuple2(env, atom_ok, term);
}

// Copyright (c) 2014-2015, Loïc Hoguin <essen@ninenines.eu>
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
#include "SDL_version.h"
#include "SDL_revision.h"

// get_version

NIF_FUNCTION(get_version)
{
	SDL_version v;

	SDL_VERSION(&v);

	return enif_make_tuple3(env,
		enif_make_int(env, v.major),
		enif_make_int(env, v.minor),
		enif_make_int(env, v.patch)
	);
}

// get_revision

NIF_FUNCTION(get_revision)
{
	return enif_make_string(env, SDL_REVISION, ERL_NIF_LATIN1);
}

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

// get_clipboard_text

NIF_FUNCTION(get_clipboard_text)
{
	ErlNifBinary bin;
	char* text;

	text = SDL_GetClipboardText();

	if (!text)
		return sdl_error_tuple(env);

	enif_alloc_binary(strlen(text), &bin);
	memcpy(bin.data, text, bin.size);

	SDL_free(text);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_binary(env, &bin)
	);
}

// has_clipboard_text

NIF_FUNCTION(has_clipboard_text)
{
	if (SDL_HasClipboardText())
		return atom_true;

	return atom_false;
}

// set_clipboard_text

NIF_FUNCTION(set_clipboard_text)
{
	ErlNifBinary bin;
	char* text;
	int ret;

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	ret = SDL_SetClipboardText(text);

	enif_free(text);

	if (ret != 0)
		return sdl_error_tuple(env);

	return atom_ok;
}

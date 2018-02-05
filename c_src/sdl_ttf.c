// Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

// @todo These operations should probably occur in the thread.
void dtor_Font(ErlNifEnv* env, void* obj)
{
	TTF_Font* font = NIF_RES_GET(Font, obj);

	TTF_CloseFont(font);
}

// ttf_init

NIF_CALL_HANDLER(thread_ttf_init)
{
	if (TTF_Init())
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(ttf_init)
{
	return nif_thread_call(env, thread_ttf_init, 0);
}

// ttf_openfont

NIF_CALL_HANDLER(thread_ttf_open_font)
{
	TTF_Font* font;
	obj_Font* res;
	ERL_NIF_TERM term;

	font = TTF_OpenFont(args[0], (long)args[1]);

	enif_free(args[0]);

	if (!font)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Font, font, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_open_font)
{
	ErlNifBinary bin;
	char* filename;
	int ptsize;

	BADARG_IF(!enif_get_int(env, argv[1], &ptsize));

	// Getting the filename last to simplify the code due to memory allocation.

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	filename = enif_alloc(bin.size + 1);
	memcpy(filename, bin.data, bin.size);
	filename[bin.size] = '\0';

	return nif_thread_call(env, thread_ttf_open_font, 2,
		filename, ptsize);
}

// ttf_quit

NIF_CAST_HANDLER(thread_ttf_quit)
{
	TTF_Quit();
}

NIF_FUNCTION(ttf_quit)
{
	return nif_thread_cast(env, thread_ttf_quit, 0);
}

// ttf_render_utf8_solid

NIF_CALL_HANDLER(thread_ttf_render_utf8_solid)
{
	SDL_Surface* surface;
	obj_Surface* res;
	ERL_NIF_TERM term;

	surface = TTF_RenderUTF8_Solid(args[0], args[1], *(SDL_Color*)args[2]);

	enif_free(args[1]);
	enif_free(args[2]);

	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Surface, surface, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_render_utf8_solid)
{
	void* font_res;
	ErlNifBinary bin;
	char* text;
	SDL_Color* color;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_inspect_binary(env, argv[1], &bin));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	color = enif_alloc(sizeof(SDL_Color));
	if (!map_to_color(env, argv[2], color)) {
		enif_free(text);
		enif_free(color);

		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_ttf_render_utf8_solid, 3,
		NIF_RES_GET(Font, font_res), text, color);
}

// ttf_was_init

NIF_CALL_HANDLER(thread_ttf_was_init)
{
	if (TTF_WasInit())
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(ttf_was_init)
{
	return nif_thread_call(env, thread_ttf_was_init, 0);
}

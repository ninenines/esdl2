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

void dtor_Texture(ErlNifEnv* env, void* obj)
{
	SDL_DestroyTexture(NIF_RES_GET(Texture, obj));
}

// create_texture_from_surface

NIF_CALL_HANDLER(thread_create_texture_from_surface)
{
	SDL_Texture* texture;
	ERL_NIF_TERM term;

	texture = SDL_CreateTextureFromSurface(args[0], args[1]);
	if (!texture)
		return sdl_error_tuple(env);

	NIF_RES_TO_TERM(Texture, texture, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(create_texture_from_surface)
{
	void* renderer_res;
	void* surface_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_resource(env, argv[1], res_Surface, &surface_res));

	return nif_thread_call(env, thread_create_texture_from_surface, 2,
		NIF_RES_GET(Renderer, renderer_res), NIF_RES_GET(Surface, surface_res));
}

// get_texture_alpha_mod

NIF_CALL_HANDLER(thread_get_texture_alpha_mod)
{
	Uint8 alpha;

	if (SDL_GetTextureAlphaMod(args[0], &alpha))
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_uint(env, alpha)
	);
}

NIF_FUNCTION(get_texture_alpha_mod)
{
	void* texture_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Texture, &texture_res));

	return nif_thread_call(env, thread_get_texture_alpha_mod, 1,
		NIF_RES_GET(Texture, texture_res));
}

// get_texture_blend_mode

NIF_CALL_HANDLER(thread_get_texture_blend_mode)
{
	SDL_BlendMode mode;

	if (SDL_GetTextureBlendMode(args[0], &mode))
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		blend_mode_to_atom(mode)
	);
}

NIF_FUNCTION(get_texture_blend_mode)
{
	void* texture_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Texture, &texture_res));

	return nif_thread_call(env, thread_get_texture_blend_mode, 1,
		NIF_RES_GET(Texture, texture_res));
}

// get_texture_color_mod

NIF_CALL_HANDLER(thread_get_texture_color_mod)
{
	Uint8 r, g, b;

	if (SDL_GetTextureColorMod(args[0], &r, &g, &b))
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_tuple3(env,
			enif_make_uint(env, r),
			enif_make_uint(env, g),
			enif_make_uint(env, b)
		)
	);
}

NIF_FUNCTION(get_texture_color_mod)
{
	void* texture_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Texture, &texture_res));

	return nif_thread_call(env, thread_get_texture_color_mod, 1,
		NIF_RES_GET(Texture, texture_res));
}

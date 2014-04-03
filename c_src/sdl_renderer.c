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

void dtor_Renderer(ErlNifEnv* env, void* obj)
{
	SDL_DestroyRenderer(NIF_RES_GET(Renderer, obj));
}

#define RENDERER_FLAGS(F) \
	F(software, SDL_RENDERER_SOFTWARE) \
	F(accelerated, SDL_RENDERER_ACCELERATED) \
	F(present_vsync, SDL_RENDERER_PRESENTVSYNC) \
	F(target_texture, SDL_RENDERER_TARGETTEXTURE)

NIF_LIST_TO_FLAGS_FUNCTION(list_to_renderer_flags, Uint32, RENDERER_FLAGS)

int map_to_rect(ErlNifEnv* env, ERL_NIF_TERM map, SDL_Rect* rect)
{
	ERL_NIF_TERM x, y, w, h;

	if (!enif_get_map_value(env, map, atom_x, &x))
		return 0;
	if (!enif_get_map_value(env, map, atom_y, &y))
		return 0;
	if (!enif_get_map_value(env, map, atom_w, &w))
		return 0;
	if (!enif_get_map_value(env, map, atom_h, &h))
		return 0;

	if (!enif_get_int(env, x, &rect->x))
		return 0;
	if (!enif_get_int(env, y, &rect->y))
		return 0;
	if (!enif_get_int(env, w, &rect->w))
		return 0;
	if (!enif_get_int(env, h, &rect->h))
		return 0;

	return 1;
}

// create_renderer

NIF_CALL_HANDLER(thread_create_renderer)
{
	SDL_Renderer* renderer;
	ERL_NIF_TERM term;

	renderer = SDL_CreateRenderer(args[0], (long)args[1], (long)args[2]);
	if (!renderer)
		return sdl_error_tuple(env);

	NIF_RES_TO_TERM(Renderer, renderer, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(create_renderer)
{
	void* window_res;
	int index;
	Uint32 flags = 0;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_int(env, argv[1], &index));
	BADARG_IF(!list_to_renderer_flags(env, argv[2], &flags));

	return nif_thread_call(env, thread_create_renderer, 3,
		NIF_RES_GET(Window, window_res), index, flags);
}

// get_num_render_drivers

NIF_FUNCTION(get_num_render_drivers)
{
	int count = SDL_GetNumRenderDrivers();

	if (count < 0)
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_int(env, count)
	);
}

// render_clear

NIF_CALL_HANDLER(thread_render_clear)
{
	if (SDL_RenderClear(args[0]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_clear)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_render_clear, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// render_copy

NIF_CALL_HANDLER(thread_render_copy)
{
	int ret = SDL_RenderCopy(args[0], args[1], args[2], args[3]);

	enif_free(args[2]);
	enif_free(args[3]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_copy)
{
	void* renderer_res;
	void* texture_res;
	SDL_Rect *srcPtr, *dstPtr;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_resource(env, argv[1], res_Texture, &texture_res));

	if (enif_is_identical(argv[2], atom_undefined))
		srcPtr = NULL;
	else {
		BADARG_IF(!enif_is_map(env, argv[2]));

		srcPtr = (SDL_Rect*)enif_alloc(sizeof(SDL_Rect));
		if (!map_to_rect(env, argv[2], srcPtr))
			return enif_make_badarg(env);
	}

	if (enif_is_identical(argv[3], atom_undefined))
		dstPtr = NULL;
	else {
		BADARG_IF(!enif_is_map(env, argv[3]));

		dstPtr = (SDL_Rect*)enif_alloc(sizeof(SDL_Rect));
		if (!map_to_rect(env, argv[3], dstPtr))
			return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_render_copy, 4,
		NIF_RES_GET(Renderer, renderer_res), NIF_RES_GET(Texture, texture_res), srcPtr, dstPtr);
}

// render_present

NIF_CAST_HANDLER(thread_render_present)
{
	SDL_RenderPresent(args[0]);
}

NIF_FUNCTION(render_present)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_cast(env, thread_render_present, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// render_set_logical_size

NIF_CALL_HANDLER(thread_render_set_logical_size)
{
	if (SDL_RenderSetLogicalSize(args[0], (long)args[1], (long)args[2]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_set_logical_size)
{
	void* renderer_res;
	int w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &w));
	BADARG_IF(!enif_get_int(env, argv[2], &h));

	return nif_thread_call(env, thread_render_set_logical_size, 3,
		NIF_RES_GET(Renderer, renderer_res), w, h);
}

// set_render_draw_color

NIF_CALL_HANDLER(thread_set_render_draw_color)
{
	if (SDL_SetRenderDrawColor(args[0], (long)args[1], (long)args[2], (long)args[3], (long)args[4]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(set_render_draw_color)
{
	void* renderer_res;
	int r, g, b, a;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &r));
	BADARG_IF(!enif_get_int(env, argv[2], &g));
	BADARG_IF(!enif_get_int(env, argv[3], &b));
	BADARG_IF(!enif_get_int(env, argv[4], &a));
	BADARG_IF(r < 0 || r > 255 || g < 0 || g > 255
		|| b < 0 || b > 255 || a < 0 || a > 255);

	return nif_thread_call(env, thread_set_render_draw_color, 5,
		NIF_RES_GET(Renderer, renderer_res), r, g, b, a);
}

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
	enif_release_resource(NIF_RES_DEP(Renderer, obj));
}

#define RENDERER_FLAGS(F) \
	F(software, SDL_RENDERER_SOFTWARE) \
	F(accelerated, SDL_RENDERER_ACCELERATED) \
	F(present_vsync, SDL_RENDERER_PRESENTVSYNC) \
	F(target_texture, SDL_RENDERER_TARGETTEXTURE)

NIF_LIST_TO_FLAGS_FUNCTION(list_to_renderer_flags, Uint32, RENDERER_FLAGS)

#define BLEND_MODE_ENUM(E) \
	E(none, SDL_BLENDMODE_NONE) \
	E(blend, SDL_BLENDMODE_BLEND) \
	E(add, SDL_BLENDMODE_ADD) \
	E(mod, SDL_BLENDMODE_MOD)

NIF_ATOM_TO_ENUM_FUNCTION(atom_to_blend_mode, SDL_BlendMode, BLEND_MODE_ENUM)
NIF_ENUM_TO_ATOM_FUNCTION(blend_mode_to_atom, SDL_BlendMode, BLEND_MODE_ENUM)

#define FLIP_FLAGS(F) \
	F(none, SDL_FLIP_NONE) \
	F(horizontal, SDL_FLIP_HORIZONTAL) \
	F(vertical, SDL_FLIP_VERTICAL)

NIF_LIST_TO_FLAGS_FUNCTION(list_to_flip_flags, SDL_RendererFlip, FLIP_FLAGS)

int map_to_point(ErlNifEnv* env, ERL_NIF_TERM map, SDL_Point* point)
{
	ERL_NIF_TERM x, y;

	if (!enif_get_map_value(env, map, atom_x, &x))
		return 0;
	if (!enif_get_map_value(env, map, atom_y, &y))
		return 0;

	if (!enif_get_int(env, x, &point->x))
		return 0;
	if (!enif_get_int(env, y, &point->y))
		return 0;

	return 1;
}

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

	renderer = SDL_CreateRenderer(NIF_RES_GET(Window, args[0]), (long)args[1], (long)args[2]);
	if (!renderer)
		return sdl_error_tuple(env);

	enif_keep_resource(args[0]);
	NIF_RES_TO_TERM_WITH_DEP(Renderer, renderer, term, args[0]);

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
		window_res, index, flags);
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

// get_render_draw_blend_mode

NIF_CALL_HANDLER(thread_get_render_draw_blend_mode)
{
	SDL_BlendMode mode;

	if (SDL_GetRenderDrawBlendMode(args[0], &mode))
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		blend_mode_to_atom(mode)
	);
}

NIF_FUNCTION(get_render_draw_blend_mode)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_get_render_draw_blend_mode, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// get_render_draw_color

NIF_CALL_HANDLER(thread_get_render_draw_color)
{
	Uint8 r, g, b, a;

	if (SDL_GetRenderDrawColor(args[0], &r, &g, &b, &a))
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_tuple4(env,
			enif_make_uint(env, r),
			enif_make_uint(env, g),
			enif_make_uint(env, b),
			enif_make_uint(env, a)
		)
	);
}

NIF_FUNCTION(get_render_draw_color)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_get_render_draw_color, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// get_render_output_size

NIF_CALL_HANDLER(thread_get_render_output_size)
{
	int w, h;

	if (SDL_GetRendererOutputSize(args[0], &w, &h))
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_tuple2(env,
			enif_make_int(env, w),
			enif_make_int(env, h)
		)
	);
}

NIF_FUNCTION(get_render_output_size)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_get_render_output_size, 1,
		NIF_RES_GET(Renderer, renderer_res));
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
	SDL_Rect *srcPtr = NULL, *dstPtr = NULL;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_resource(env, argv[1], res_Texture, &texture_res));

	if (!enif_is_identical(argv[2], atom_undefined)) {
		BADARG_IF(!enif_is_map(env, argv[2]));

		srcPtr = (SDL_Rect*)enif_alloc(sizeof(SDL_Rect));
		if (!map_to_rect(env, argv[2], srcPtr))
			goto render_copy_badarg;
	}

	if (!enif_is_identical(argv[3], atom_undefined)) {
		if (!enif_is_map(env, argv[3]))
			goto render_copy_badarg;

		dstPtr = (SDL_Rect*)enif_alloc(sizeof(SDL_Rect));
		if (!map_to_rect(env, argv[3], dstPtr))
			goto render_copy_badarg;
	}

	return nif_thread_call(env, thread_render_copy, 4,
		NIF_RES_GET(Renderer, renderer_res), NIF_RES_GET(Texture, texture_res), srcPtr, dstPtr);

render_copy_badarg:
	enif_free(srcPtr);
	enif_free(dstPtr);

	return enif_make_badarg(env);
}

// render_copy_ex

NIF_CALL_HANDLER(thread_render_copy_ex)
{
	int ret = SDL_RenderCopyEx(args[0], args[1], args[2], args[3],
		*((double*)args[4]), args[5], (long)args[6]);

	enif_free(args[2]);
	enif_free(args[3]);
	enif_free(args[4]);
	enif_free(args[5]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_copy_ex)
{
	void* renderer_res;
	void* texture_res;
	SDL_Rect *srcPtr = NULL, *dstPtr = NULL;
	double *anglePtr = NULL;
	SDL_Point* centerPtr = NULL;
	SDL_RendererFlip flip;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_resource(env, argv[1], res_Texture, &texture_res));

	if (!enif_is_identical(argv[2], atom_undefined)) {
		BADARG_IF(!enif_is_map(env, argv[2]));

		srcPtr = (SDL_Rect*)enif_alloc(sizeof(SDL_Rect));
		if (!map_to_rect(env, argv[2], srcPtr))
			goto render_copy_ex_badarg;
	}

	if (!enif_is_identical(argv[3], atom_undefined)) {
		if (!enif_is_map(env, argv[3]))
			goto render_copy_ex_badarg;

		dstPtr = (SDL_Rect*)enif_alloc(sizeof(SDL_Rect));
		if (!map_to_rect(env, argv[3], dstPtr))
			goto render_copy_ex_badarg;
	}

	anglePtr = (double*)enif_alloc(sizeof(double));
	if (!enif_get_double(env, argv[4], anglePtr))
		goto render_copy_ex_badarg;

	if (!enif_is_identical(argv[5], atom_undefined)) {
		if (!enif_is_map(env, argv[5]))
			goto render_copy_ex_badarg;

		centerPtr = (SDL_Point*)enif_alloc(sizeof(SDL_Point));
		if (!map_to_point(env, argv[5], centerPtr))
			goto render_copy_ex_badarg;
	}

	if (!list_to_flip_flags(env, argv[6], &flip))
		goto render_copy_ex_badarg;

	return nif_thread_call(env, thread_render_copy_ex, 7,
		NIF_RES_GET(Renderer, renderer_res), NIF_RES_GET(Texture, texture_res), srcPtr, dstPtr,
		anglePtr, centerPtr, flip);

render_copy_ex_badarg:
	enif_free(srcPtr);
	enif_free(dstPtr);
	enif_free(anglePtr);
	enif_free(centerPtr);

	return enif_make_badarg(env);
}

// render_draw_line

NIF_CALL_HANDLER(thread_render_draw_line)
{
	if (SDL_RenderDrawLine(args[0], (long)args[1], (long)args[2], (long)args[3], (long)args[4]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_draw_line)
{
	void* renderer_res;
	int x1, y1, x2, y2;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x1));
	BADARG_IF(!enif_get_int(env, argv[2], &y1));
	BADARG_IF(!enif_get_int(env, argv[3], &x2));
	BADARG_IF(!enif_get_int(env, argv[4], &y2));

	return nif_thread_call(env, thread_render_draw_line, 5,
		NIF_RES_GET(Renderer, renderer_res), x1, y1, x2, y2);
}

// render_draw_lines

NIF_CALL_HANDLER(thread_render_draw_lines)
{
	int ret;

	ret = SDL_RenderDrawLines(args[0], args[1], (long)args[2]);

	enif_free(args[1]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_draw_lines)
{
	void* renderer_res;
	unsigned int len;
	SDL_Point* points;
	ERL_NIF_TERM list, head;
	int i = 0;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_list_length(env, argv[1], &len));
	BADARG_IF(len < 2);

	points = (SDL_Point*)enif_alloc(len * sizeof(SDL_Point));

	list = argv[1];
	while (enif_get_list_cell(env, list, &head, &list)) {
		if (!map_to_point(env, head, &(points[i++]))) {
			enif_free(points);
			return enif_make_badarg(env);
		}
	}

	return nif_thread_call(env, thread_render_draw_lines, 3,
		NIF_RES_GET(Renderer, renderer_res), points, len);
}

// render_draw_point

NIF_CALL_HANDLER(thread_render_draw_point)
{
	if (SDL_RenderDrawPoint(args[0], (long)args[1], (long)args[2]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_draw_point)
{
	void* renderer_res;
	int x, y;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));

	return nif_thread_call(env, thread_render_draw_point, 3,
		NIF_RES_GET(Renderer, renderer_res), x, y);
}

// render_draw_points

NIF_CALL_HANDLER(thread_render_draw_points)
{
	int ret;

	ret = SDL_RenderDrawPoints(args[0], args[1], (long)args[2]);

	enif_free(args[1]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_draw_points)
{
	void* renderer_res;
	unsigned int len;
	SDL_Point* points;
	ERL_NIF_TERM list, head;
	int i = 0;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_list_length(env, argv[1], &len));
	BADARG_IF(len < 2);

	points = (SDL_Point*)enif_alloc(len * sizeof(SDL_Point));

	list = argv[1];
	while (enif_get_list_cell(env, list, &head, &list)) {
		if (!map_to_point(env, head, &(points[i++]))) {
			enif_free(points);
			return enif_make_badarg(env);
		}
	}

	return nif_thread_call(env, thread_render_draw_points, 3,
		NIF_RES_GET(Renderer, renderer_res), points, len);
}

// render_draw_rect

NIF_CALL_HANDLER(thread_render_draw_rect)
{
	SDL_Rect rect = {(long)args[1], (long)args[2], (long)args[3], (long)args[4]};

	if (SDL_RenderDrawRect(args[0], &rect))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_draw_rect)
{
	void* renderer_res;
	int x, y, w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));
	BADARG_IF(!enif_get_int(env, argv[3], &w));
	BADARG_IF(!enif_get_int(env, argv[4], &h));

	return nif_thread_call(env, thread_render_draw_rect, 5,
		NIF_RES_GET(Renderer, renderer_res), x, y, w, h);
}

// render_draw_rects

NIF_CALL_HANDLER(thread_render_draw_rects)
{
	int ret;

	ret = SDL_RenderDrawRects(args[0], args[1], (long)args[2]);

	enif_free(args[1]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_draw_rects)
{
	void* renderer_res;
	unsigned int len;
	SDL_Rect* rects;
	ERL_NIF_TERM list, head;
	int i = 0;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_list_length(env, argv[1], &len));
	BADARG_IF(len < 2);

	rects = (SDL_Rect*)enif_alloc(len * sizeof(SDL_Rect));

	list = argv[1];
	while (enif_get_list_cell(env, list, &head, &list)) {
		if (!map_to_rect(env, head, &(rects[i++]))) {
			enif_free(rects);
			return enif_make_badarg(env);
		}
	}

	return nif_thread_call(env, thread_render_draw_rects, 3,
		NIF_RES_GET(Renderer, renderer_res), rects, len);
}

// render_fill_rect

NIF_CALL_HANDLER(thread_render_fill_rect)
{
	SDL_Rect rect = {(long)args[1], (long)args[2], (long)args[3], (long)args[4]};

	if (SDL_RenderFillRect(args[0], &rect))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_fill_rect)
{
	void* renderer_res;
	int x, y, w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));
	BADARG_IF(!enif_get_int(env, argv[3], &w));
	BADARG_IF(!enif_get_int(env, argv[4], &h));

	return nif_thread_call(env, thread_render_fill_rect, 5,
		NIF_RES_GET(Renderer, renderer_res), x, y, w, h);
}

// render_fill_rects

NIF_CALL_HANDLER(thread_render_fill_rects)
{
	int ret;

	ret = SDL_RenderFillRects(args[0], args[1], (long)args[2]);

	enif_free(args[1]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_fill_rects)
{
	void* renderer_res;
	unsigned int len;
	SDL_Rect* rects;
	ERL_NIF_TERM list, head;
	int i = 0;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_list_length(env, argv[1], &len));
	BADARG_IF(len < 2);

	rects = (SDL_Rect*)enif_alloc(len * sizeof(SDL_Rect));

	list = argv[1];
	while (enif_get_list_cell(env, list, &head, &list)) {
		if (!map_to_rect(env, head, &(rects[i++]))) {
			enif_free(rects);
			return enif_make_badarg(env);
		}
	}

	return nif_thread_call(env, thread_render_fill_rects, 3,
		NIF_RES_GET(Renderer, renderer_res), rects, len);
}

// render_get_clip_rect

NIF_CALL_HANDLER(thread_render_get_clip_rect)
{
	SDL_Rect rect;
	ERL_NIF_TERM map;

	SDL_RenderGetClipRect(args[0], &rect);

	map = enif_make_new_map(env);

	enif_make_map_put(env, map, atom_x, enif_make_int(env, rect.x), &map);
	enif_make_map_put(env, map, atom_y, enif_make_int(env, rect.y), &map);
	enif_make_map_put(env, map, atom_w, enif_make_int(env, rect.w), &map);
	enif_make_map_put(env, map, atom_h, enif_make_int(env, rect.h), &map);

	return map;
}

NIF_FUNCTION(render_get_clip_rect)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_render_get_clip_rect, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// render_get_logical_size

NIF_CALL_HANDLER(thread_render_get_logical_size)
{
	SDL_Rect rect;
	int w, h;

	SDL_RenderGetLogicalSize(args[0], &w, &h);

	return enif_make_tuple2(env,
		enif_make_int(env, w),
		enif_make_int(env, h)
	);
}

NIF_FUNCTION(render_get_logical_size)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_render_get_logical_size, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// render_get_scale

NIF_CALL_HANDLER(thread_render_get_scale)
{
	SDL_Rect rect;
	float scaleX, scaleY;

	SDL_RenderGetScale(args[0], &scaleX, &scaleY);

	return enif_make_tuple2(env,
		enif_make_double(env, scaleX),
		enif_make_double(env, scaleY)
	);
}

NIF_FUNCTION(render_get_scale)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_render_get_scale, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// render_get_viewport

NIF_CALL_HANDLER(thread_render_get_viewport)
{
	SDL_Rect rect;
	ERL_NIF_TERM map;

	SDL_RenderGetViewport(args[0], &rect);

	map = enif_make_new_map(env);

	enif_make_map_put(env, map, atom_x, enif_make_int(env, rect.x), &map);
	enif_make_map_put(env, map, atom_y, enif_make_int(env, rect.y), &map);
	enif_make_map_put(env, map, atom_w, enif_make_int(env, rect.w), &map);
	enif_make_map_put(env, map, atom_h, enif_make_int(env, rect.h), &map);

	return map;
}

NIF_FUNCTION(render_get_viewport)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_render_get_viewport, 1,
		NIF_RES_GET(Renderer, renderer_res));
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

// render_set_clip_rect

NIF_CALL_HANDLER(thread_render_set_clip_rect)
{
	SDL_Rect rect = {(long)args[1], (long)args[2], (long)args[3], (long)args[4]};

	if (SDL_RenderSetClipRect(args[0], &rect))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_set_clip_rect)
{
	void* renderer_res;
	int x, y, w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));
	BADARG_IF(!enif_get_int(env, argv[3], &w));
	BADARG_IF(!enif_get_int(env, argv[4], &h));

	return nif_thread_call(env, thread_render_set_clip_rect, 5,
		NIF_RES_GET(Renderer, renderer_res), x, y, w, h);
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

// render_set_scale

NIF_CALL_HANDLER(thread_render_set_scale)
{
	int ret;

	ret = SDL_RenderSetScale(args[0], *((double*)args[1]), *((double*)args[2]));

	enif_free(args[1]);
	enif_free(args[2]);

	if (ret)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_set_scale)
{
	void* renderer_res;
	double *scaleXPtr, *scaleYPtr;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	scaleXPtr = (double*)enif_alloc(sizeof(double));
	if (!enif_get_double(env, argv[1], scaleXPtr)) {
		enif_free(scaleXPtr);
		return enif_make_badarg(env);
	}

	scaleYPtr = (double*)enif_alloc(sizeof(double));
	if (!enif_get_double(env, argv[2], scaleYPtr)) {
		enif_free(scaleXPtr);
		enif_free(scaleYPtr);
		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_render_set_scale, 3,
		NIF_RES_GET(Renderer, renderer_res), scaleXPtr, scaleYPtr);
}

// render_set_viewport

NIF_CALL_HANDLER(thread_render_set_viewport)
{
	SDL_Rect rect = {(long)args[1], (long)args[2], (long)args[3], (long)args[4]};

	if (SDL_RenderSetViewport(args[0], &rect))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(render_set_viewport)
{
	void* renderer_res;
	int x, y, w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));
	BADARG_IF(!enif_get_int(env, argv[3], &w));
	BADARG_IF(!enif_get_int(env, argv[4], &h));

	return nif_thread_call(env, thread_render_set_viewport, 5,
		NIF_RES_GET(Renderer, renderer_res), x, y, w, h);
}

// get_window_grab

NIF_CALL_HANDLER(thread_render_target_supported)
{
	if (SDL_RenderTargetSupported(args[0]))
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(render_target_supported)
{
	void* renderer_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));

	return nif_thread_call(env, thread_render_target_supported, 1,
		NIF_RES_GET(Renderer, renderer_res));
}

// set_render_draw_blend_mode

NIF_CALL_HANDLER(thread_set_render_draw_blend_mode)
{
	if (SDL_SetRenderDrawBlendMode(args[0], (long)args[1]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(set_render_draw_blend_mode)
{
	void* renderer_res;
	SDL_BlendMode mode;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Renderer, &renderer_res));
	BADARG_IF(!atom_to_blend_mode(env, argv[1], &mode));

	return nif_thread_call(env, thread_set_render_draw_blend_mode, 2,
		NIF_RES_GET(Renderer, renderer_res), mode);
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

	return nif_thread_call(env, thread_set_render_draw_color, 5,
		NIF_RES_GET(Renderer, renderer_res), r, g, b, a);
}

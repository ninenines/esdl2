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

#define PIXEL_FORMAT_ENUM(E) \
	E(unknown, SDL_PIXELFORMAT_UNKNOWN) \
	E(index1lsb, SDL_PIXELFORMAT_INDEX1LSB) \
	E(index1msb, SDL_PIXELFORMAT_INDEX1MSB) \
	E(index4lsb, SDL_PIXELFORMAT_INDEX4LSB) \
	E(index4msb, SDL_PIXELFORMAT_INDEX4MSB) \
	E(index8, SDL_PIXELFORMAT_INDEX8) \
	E(rgb332, SDL_PIXELFORMAT_RGB332) \
	E(rgb444, SDL_PIXELFORMAT_RGB444) \
	E(rgb555, SDL_PIXELFORMAT_RGB555) \
	E(bgr555, SDL_PIXELFORMAT_BGR555) \
	E(argb4444, SDL_PIXELFORMAT_ARGB4444) \
	E(rgba4444, SDL_PIXELFORMAT_RGBA4444) \
	E(abgr4444, SDL_PIXELFORMAT_ABGR4444) \
	E(bgra4444, SDL_PIXELFORMAT_BGRA4444) \
	E(argb1555, SDL_PIXELFORMAT_ARGB1555) \
	E(rgba5551, SDL_PIXELFORMAT_RGBA5551) \
	E(abgr1555, SDL_PIXELFORMAT_ABGR1555) \
	E(bgra5551, SDL_PIXELFORMAT_BGRA5551) \
	E(rgb565, SDL_PIXELFORMAT_RGB565) \
	E(bgr565, SDL_PIXELFORMAT_BGR565) \
	E(rgb24, SDL_PIXELFORMAT_RGB24) \
	E(bgr24, SDL_PIXELFORMAT_BGR24) \
	E(rgb888, SDL_PIXELFORMAT_RGB888) \
	E(rgbx8888, SDL_PIXELFORMAT_RGBX8888) \
	E(bgr888, SDL_PIXELFORMAT_BGR888) \
	E(bgrx8888, SDL_PIXELFORMAT_BGRX8888) \
	E(argb8888, SDL_PIXELFORMAT_ARGB8888) \
	E(rgba8888, SDL_PIXELFORMAT_RGBA8888) \
	E(abgr8888, SDL_PIXELFORMAT_ABGR8888) \
	E(bgra8888, SDL_PIXELFORMAT_BGRA8888) \
	E(argb2101010, SDL_PIXELFORMAT_ARGB2101010) \
	E(rgba32, SDL_PIXELFORMAT_RGBA32) \
	E(argb32, SDL_PIXELFORMAT_ARGB32) \
	E(bgra32, SDL_PIXELFORMAT_BGRA32) \
	E(abgr32, SDL_PIXELFORMAT_ABGR32) \
	E(yv12, SDL_PIXELFORMAT_YV12) \
	E(iyuv, SDL_PIXELFORMAT_IYUV) \
	E(yuy2, SDL_PIXELFORMAT_YUY2) \
	E(uyvy, SDL_PIXELFORMAT_UYVY) \
	E(yvyu, SDL_PIXELFORMAT_YVYU) \
	E(nv12, SDL_PIXELFORMAT_NV12) \
	E(nv21, SDL_PIXELFORMAT_NV21)

NIF_ENUM_TO_ATOM_FUNCTION(pixel_format_to_atom, Uint32, PIXEL_FORMAT_ENUM)
NIF_ATOM_TO_ENUM_FUNCTION(atom_to_pixel_format, Uint32, PIXEL_FORMAT_ENUM)

int map_to_color(ErlNifEnv* env, ERL_NIF_TERM map, SDL_Color* color)
{
	ERL_NIF_TERM r, g, b, a;
	unsigned int ri, gi, bi, ai;

	if (!enif_get_map_value(env, map, atom_r, &r))
		return 0;
	if (!enif_get_map_value(env, map, atom_g, &g))
		return 0;
	if (!enif_get_map_value(env, map, atom_b, &b))
		return 0;
	if (!enif_get_map_value(env, map, atom_a, &a))
		return 0;

	if (!enif_get_uint(env, r, &ri))
		return 0;
	if (!enif_get_uint(env, g, &gi))
		return 0;
	if (!enif_get_uint(env, b, &bi))
		return 0;
	if (!enif_get_uint(env, a, &ai))
		return 0;

	color->r = ri;
	color->g = gi;
	color->b = bi;
	color->a = ai;

	return 1;
}

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

ERL_NIF_TERM point_to_map(ErlNifEnv* env, const SDL_Point* point)
{
	ERL_NIF_TERM map;

	map = enif_make_new_map(env);

	enif_make_map_put(env, map, atom_x,
		enif_make_int(env, point->x), &map);
	enif_make_map_put(env, map, atom_y,
		enif_make_int(env, point->y), &map);

	return map;
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

ERL_NIF_TERM rect_to_map(ErlNifEnv* env, const SDL_Rect* rect)
{
	ERL_NIF_TERM map;

	map = enif_make_new_map(env);

	enif_make_map_put(env, map, atom_x,
		enif_make_int(env, rect->x), &map);
	enif_make_map_put(env, map, atom_y,
		enif_make_int(env, rect->y), &map);
	enif_make_map_put(env, map, atom_w,
		enif_make_int(env, rect->w), &map);
	enif_make_map_put(env, map, atom_h,
		enif_make_int(env, rect->h), &map);

	return map;
}

// enclose_points

NIF_FUNCTION(enclose_points)
{
	SDL_Point* points = NULL;
	SDL_Rect clip, result;
	SDL_bool b;
	ERL_NIF_TERM list, head;
	unsigned int count;
	int i = 0;

	BADARG_IF(!enif_get_list_length(env, argv[0], &count));
	BADARG_IF(!map_to_rect(env, argv[1], &clip));

	points = (SDL_Point*)enif_alloc(count * sizeof(SDL_Point));

	list = argv[0];
	while (enif_get_list_cell(env, list, &head, &list)) {
		if (!map_to_point(env, head, &(points[i++]))) {
			enif_free(points);
			return enif_make_badarg(env);
		}
	}

	b = SDL_EnclosePoints(points, count, &clip, &result);

	enif_free(points);

	if (!b)
		return atom_false;

	return rect_to_map(env, &result);
}

// has_intersection

NIF_FUNCTION(has_intersection)
{
	SDL_Rect a, b;

	BADARG_IF(!map_to_rect(env, argv[0], &a));
	BADARG_IF(!map_to_rect(env, argv[1], &b));

	if (SDL_HasIntersection(&a, &b))
		return atom_true;

	return atom_false;
}

// intersect_rect

NIF_FUNCTION(intersect_rect)
{
	SDL_Rect a, b, result;

	BADARG_IF(!map_to_rect(env, argv[0], &a));
	BADARG_IF(!map_to_rect(env, argv[1], &b));

	if (!SDL_IntersectRect(&a, &b, &result))
		return atom_false;

	return rect_to_map(env, &result);
}

// intersect_rect_and_line

NIF_FUNCTION(intersect_rect_and_line)
{
	SDL_Rect rect;
	int x1, y1, x2, y2;

	BADARG_IF(!map_to_rect(env, argv[0], &rect));
	BADARG_IF(!enif_get_int(env, argv[1], &x1));
	BADARG_IF(!enif_get_int(env, argv[2], &y1));
	BADARG_IF(!enif_get_int(env, argv[3], &x2));
	BADARG_IF(!enif_get_int(env, argv[4], &y2));

	if (!SDL_IntersectRectAndLine(&rect, &x1, &y1, &x2, &y2))
		return atom_false;

	return enif_make_tuple4(env,
		enif_make_int(env, x1),
		enif_make_int(env, y1),
		enif_make_int(env, x2),
		enif_make_int(env, y2));
}

// union_rect

NIF_FUNCTION(union_rect)
{
	SDL_Rect a, b, result;

	BADARG_IF(!map_to_rect(env, argv[0], &a));
	BADARG_IF(!map_to_rect(env, argv[1], &b));

	SDL_UnionRect(&a, &b, &result);

	return rect_to_map(env, &result);
}

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

// get_current_video_driver

NIF_CALL_HANDLER(thread_get_current_video_driver)
{
	const char* name;
	ErlNifBinary bin;

	name = SDL_GetCurrentVideoDriver();

	if (!name)
		return atom_undefined;

	enif_alloc_binary(strlen(name), &bin);
	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

NIF_FUNCTION(get_current_video_driver)
{
	return nif_thread_call(env, thread_get_current_video_driver, 0);
}

// get_display_bounds

NIF_CALL_HANDLER(thread_get_display_bounds)
{
	SDL_Rect rect;

	if (SDL_GetDisplayBounds((long)args[0], &rect))
		return atom_undefined;

	return rect_to_map(env, &rect);
}

NIF_FUNCTION(get_display_bounds)
{
	int index;

	BADARG_IF(!enif_get_int(env, argv[0], &index));

	return nif_thread_call(env, thread_get_display_bounds, 1, index);
}

// get_display_dpi

NIF_CALL_HANDLER(thread_get_display_dpi)
{
	float ddpi, hdpi, vdpi;
	ERL_NIF_TERM map;

	if (SDL_GetDisplayDPI((long)args[0], &ddpi, &hdpi, &vdpi))
		return atom_undefined;

	map = enif_make_new_map(env);

	enif_make_map_put(env, map, atom_diagonal,
		enif_make_double(env, ddpi), &map);
	enif_make_map_put(env, map, atom_horizontal,
		enif_make_double(env, hdpi), &map);
	enif_make_map_put(env, map, atom_vertical,
		enif_make_double(env, vdpi), &map);

	return map;
}

NIF_FUNCTION(get_display_dpi)
{
	int index;

	BADARG_IF(!enif_get_int(env, argv[0], &index));

	return nif_thread_call(env, thread_get_display_dpi, 1, index);
}

// get_display_name

NIF_CALL_HANDLER(thread_get_display_name)
{
	const char* name;
	ErlNifBinary bin;

	name = SDL_GetDisplayName((long)args[0]);

	if (!name)
		return atom_undefined;

	enif_alloc_binary(strlen(name), &bin);
	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

NIF_FUNCTION(get_display_name)
{
	int index;

	BADARG_IF(!enif_get_int(env, argv[0], &index));

	return nif_thread_call(env, thread_get_display_name, 1, index);
}

// get_display_usable_bounds

NIF_CALL_HANDLER(thread_get_display_usable_bounds)
{
	SDL_Rect rect;

	if (SDL_GetDisplayUsableBounds((long)args[0], &rect))
		return atom_undefined;

	return rect_to_map(env, &rect);
}

NIF_FUNCTION(get_display_usable_bounds)
{
	int index;

	BADARG_IF(!enif_get_int(env, argv[0], &index));

	return nif_thread_call(env, thread_get_display_usable_bounds, 1, index);
}

// get_num_video_displays

NIF_CALL_HANDLER(thread_get_num_video_displays)
{
	return enif_make_int(env, SDL_GetNumVideoDisplays());
}

NIF_FUNCTION(get_num_video_displays)
{
	return nif_thread_call(env, thread_get_num_video_displays, 0);
}

// get_num_video_drivers

NIF_FUNCTION(get_num_video_drivers)
{
	return enif_make_int(env, SDL_GetNumVideoDrivers());
}

// get_video_driver

NIF_FUNCTION(get_video_driver)
{
	int index;
	const char* name;
	ErlNifBinary bin;

	BADARG_IF(!enif_get_int(env, argv[0], &index));

	name = SDL_GetVideoDriver(index);

	if (!name)
		return atom_undefined;

	enif_alloc_binary(strlen(name), &bin);
	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

// video_init

NIF_CALL_HANDLER(thread_video_init)
{
	int result;

	result = SDL_VideoInit(args[0]);

	free(args[0]);

	if (result)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(video_init)
{
	ErlNifBinary bin;
	char* name;

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	name = malloc(bin.size + 1);
	memcpy(name, bin.data, bin.size);
	name[bin.size] = '\0';

	return nif_thread_call(env, thread_video_init, 1, name);
}

// video_quit

NIF_CAST_HANDLER(thread_video_quit)
{
	SDL_VideoQuit();
}

NIF_FUNCTION(video_quit)
{
	return nif_thread_cast(env, thread_video_quit, 0);
}

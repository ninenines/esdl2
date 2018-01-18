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

ERL_NIF_TERM display_mode_to_map(ErlNifEnv* env, SDL_DisplayMode* mode)
{
	ERL_NIF_TERM map;

	map = enif_make_new_map(env);

	enif_make_map_put(env, map, atom_format,
		pixel_format_to_atom(mode->format), &map);
	enif_make_map_put(env, map, atom_w,
		enif_make_int(env, mode->w), &map);
	enif_make_map_put(env, map, atom_h,
		enif_make_int(env, mode->h), &map);
	enif_make_map_put(env, map, atom_refresh_rate,
		enif_make_int(env, mode->refresh_rate), &map);

	// There is also a driverdata field but it is unclear whether it has any use.

	return map;
}

// disable_screensaver

NIF_CAST_HANDLER(thread_disable_screensaver)
{
	SDL_DisableScreenSaver();
}

NIF_FUNCTION(disable_screensaver)
{
	return nif_thread_cast(env, thread_disable_screensaver, 0);
}

// enable_screensaver

NIF_CAST_HANDLER(thread_enable_screensaver)
{
	SDL_EnableScreenSaver();
}

NIF_FUNCTION(enable_screensaver)
{
	return nif_thread_cast(env, thread_enable_screensaver, 0);
}

// get_closest_display_mode

NIF_CALL_HANDLER(thread_get_closest_display_mode)
{
	SDL_DisplayMode mode, closest;

	mode.format = (long)args[1];
	mode.w = (long)args[2];
	mode.h = (long)args[3];
	mode.refresh_rate = (long)args[4];

	if (!SDL_GetClosestDisplayMode((long)args[0], &mode, &closest))
		return atom_undefined;

	return display_mode_to_map(env, &closest);
}

NIF_FUNCTION(get_closest_display_mode)
{
	int displayIndex;
	ERL_NIF_TERM term;
	Uint32 format = 0;
	int w = 0, h = 0, refresh_rate = 0;

	BADARG_IF(!enif_get_int(env, argv[0], &displayIndex));
	BADARG_IF(!enif_is_map(env, argv[1]));

	// We default to 0 when a field is missing.

	if (enif_get_map_value(env, argv[1], atom_format, &term))
		BADARG_IF(!atom_to_pixel_format(env, term, &format));

	if (enif_get_map_value(env, argv[1], atom_w, &term))
		BADARG_IF(!enif_get_int(env, term, &w));

	if (enif_get_map_value(env, argv[1], atom_h, &term))
		BADARG_IF(!enif_get_int(env, term, &h));

	if (enif_get_map_value(env, argv[1], atom_refresh_rate, &term))
		BADARG_IF(!enif_get_int(env, term, &refresh_rate));

	return nif_thread_call(env, thread_get_closest_display_mode, 5,
		displayIndex, format, w, h, refresh_rate);
}

// get_current_display_mode

NIF_CALL_HANDLER(thread_get_current_display_mode)
{
	SDL_DisplayMode mode;

	if (SDL_GetCurrentDisplayMode((long)args[0], &mode))
		return atom_undefined;

	return display_mode_to_map(env, &mode);
}

NIF_FUNCTION(get_current_display_mode)
{
	int displayIndex;

	BADARG_IF(!enif_get_int(env, argv[0], &displayIndex));

	return nif_thread_call(env, thread_get_current_display_mode, 1, displayIndex);
}

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

// get_desktop_display_mode

NIF_CALL_HANDLER(thread_get_desktop_display_mode)
{
	SDL_DisplayMode mode;

	if (SDL_GetDesktopDisplayMode((long)args[0], &mode))
		return atom_undefined;

	return display_mode_to_map(env, &mode);
}

NIF_FUNCTION(get_desktop_display_mode)
{
	int displayIndex;

	BADARG_IF(!enif_get_int(env, argv[0], &displayIndex));

	return nif_thread_call(env, thread_get_desktop_display_mode, 1, displayIndex);
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

// get_display_mode

NIF_CALL_HANDLER(thread_get_display_mode)
{
	SDL_DisplayMode mode;

	if (SDL_GetDisplayMode((long)args[0], (long)args[1], &mode))
		return atom_undefined;

	return display_mode_to_map(env, &mode);
}

NIF_FUNCTION(get_display_mode)
{
	int displayIndex, index;

	BADARG_IF(!enif_get_int(env, argv[0], &displayIndex));
	BADARG_IF(!enif_get_int(env, argv[1], &index));

	return nif_thread_call(env, thread_get_display_mode, 2,
		displayIndex, index);
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

// get_num_display_modes

NIF_CALL_HANDLER(thread_get_num_display_modes)
{
	return enif_make_int(env, SDL_GetNumDisplayModes((long)args[0]));
}

NIF_FUNCTION(get_num_display_modes)
{
	int displayIndex;

	BADARG_IF(!enif_get_int(env, argv[0], &displayIndex));

	return nif_thread_call(env, thread_get_num_display_modes, 1, displayIndex);
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

// is_screensaver_enabled

NIF_CALL_HANDLER(thread_is_screensaver_enabled)
{
	if (SDL_IsScreenSaverEnabled())
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(is_screensaver_enabled)
{
	return nif_thread_call(env, thread_is_screensaver_enabled, 0);
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

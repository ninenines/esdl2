// Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

#define MOUSEWHEEL_DIRECTION_ENUM(E) \
	E(normal, SDL_MOUSEWHEEL_NORMAL) \
	E(flipped, SDL_MOUSEWHEEL_FLIPPED)

NIF_ENUM_TO_ATOM_FUNCTION(mousewheel_direction_to_atom, Uint32, MOUSEWHEEL_DIRECTION_ENUM)

static ERL_NIF_TERM get_mouse_state_common(ErlNifEnv* env, int x, int y, Uint32 state)
{
	ERL_NIF_TERM list;

	list = enif_make_list(env, 0);

	if (state & SDL_BUTTON_LMASK)
		list = enif_make_list_cell(env, atom_left, list);
	if (state & SDL_BUTTON_MMASK)
		list = enif_make_list_cell(env, atom_middle, list);
	if (state & SDL_BUTTON_RMASK)
		list = enif_make_list_cell(env, atom_right, list);
	if (state & SDL_BUTTON_X1MASK)
		list = enif_make_list_cell(env, atom_x1, list);
	if (state & SDL_BUTTON_X2MASK)
		list = enif_make_list_cell(env, atom_x2, list);

	return enif_make_tuple3(env,
		enif_make_int(env, x),
		enif_make_int(env, y),
		list
	);
}

// capture_mouse

NIF_CALL_HANDLER(thread_capture_mouse)
{
	if (SDL_CaptureMouse((long)args[0]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(capture_mouse)
{
	SDL_bool b;

	BADARG_IF(!atom_to_bool(env, argv[0], &b));

	return nif_thread_call(env, thread_capture_mouse, 1, b);
}

// get_global_mouse_state

NIF_CALL_HANDLER(thread_get_global_mouse_state)
{
	Uint32 state;
	int x, y;

	state = SDL_GetGlobalMouseState(&x, &y);

	return get_mouse_state_common(env, x, y, state);
}

NIF_FUNCTION(get_global_mouse_state)
{
	return nif_thread_call(env, thread_get_global_mouse_state, 0);
}

// get_mouse_focus

NIF_CALL_HANDLER(thread_get_mouse_focus)
{
	SDL_Window* window;

	window = SDL_GetMouseFocus();

	if (!window)
		return atom_undefined;

	return esdl2_windows_find(env, window);
}

NIF_FUNCTION(get_mouse_focus)
{
	return nif_thread_call(env, thread_get_mouse_focus, 0);
}

// get_mouse_state

NIF_CALL_HANDLER(thread_get_mouse_state)
{
	Uint32 state;
	int x, y;

	state = SDL_GetMouseState(&x, &y);

	return get_mouse_state_common(env, x, y, state);
}

NIF_FUNCTION(get_mouse_state)
{
	return nif_thread_call(env, thread_get_mouse_state, 0);
}

// get_relative_mouse_mode

NIF_CALL_HANDLER(thread_get_relative_mouse_mode)
{
	if (SDL_GetRelativeMouseMode())
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(get_relative_mouse_mode)
{
	return nif_thread_call(env, thread_get_relative_mouse_mode, 0);
}

// get_relative_mouse_state

NIF_CALL_HANDLER(thread_get_relative_mouse_state)
{
	Uint32 state;
	int x, y;

	state = SDL_GetRelativeMouseState(&x, &y);

	return get_mouse_state_common(env, x, y, state);
}

NIF_FUNCTION(get_relative_mouse_state)
{
	return nif_thread_call(env, thread_get_relative_mouse_state, 0);
}

// set_relative_mouse_mode

NIF_CALL_HANDLER(thread_set_relative_mouse_mode)
{
	if (SDL_SetRelativeMouseMode((long)args[0]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(set_relative_mouse_mode)
{
	SDL_bool b;

	BADARG_IF(!atom_to_bool(env, argv[0], &b));

	return nif_thread_call(env, thread_set_relative_mouse_mode, 1, b);
}

// warp_mouse_global

NIF_CALL_HANDLER(thread_warp_mouse_global)
{
	if (SDL_WarpMouseGlobal((long)args[0], (long)args[1]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(warp_mouse_global)
{
	int x, y;

	BADARG_IF(!enif_get_int(env, argv[0], &x));
	BADARG_IF(!enif_get_int(env, argv[1], &y));

	return nif_thread_call(env, thread_warp_mouse_global, 2, x, y);
}

// warp_mouse_in_window

NIF_CAST_HANDLER(thread_warp_mouse_in_window)
{
	SDL_WarpMouseInWindow(args[0], (long)args[1], (long)args[2]);
}

NIF_FUNCTION(warp_mouse_in_window)
{
	void* window_res;
	int x, y;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));

	return nif_thread_cast(env, thread_warp_mouse_in_window, 3,
		NIF_RES_GET(Window, window_res), x, y);
}

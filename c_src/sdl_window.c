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

// @todo These operations should probably occur in the thread.
void dtor_Window(ErlNifEnv* env, void* obj)
{
	SDL_Window* window = NIF_RES_GET(Window, obj);

	SDL_DestroyWindow(window);
	esdl2_windows_remove(window);
}

#define WINDOW_FLAGS(F) \
	F(fullscreen, SDL_WINDOW_FULLSCREEN) \
	F(opengl, SDL_WINDOW_OPENGL) \
	F(shown, SDL_WINDOW_SHOWN) \
	F(hidden, SDL_WINDOW_HIDDEN) \
	F(borderless, SDL_WINDOW_BORDERLESS) \
	F(resizable, SDL_WINDOW_RESIZABLE) \
	F(minimized, SDL_WINDOW_MINIMIZED) \
	F(maximized, SDL_WINDOW_MAXIMIZED) \
	F(input_grabbed, SDL_WINDOW_INPUT_GRABBED) \
	F(input_focus, SDL_WINDOW_INPUT_FOCUS) \
	F(mouse_focus, SDL_WINDOW_MOUSE_FOCUS) \
	F(fullscreen_desktop, SDL_WINDOW_FULLSCREEN_DESKTOP) \
	F(foreign, SDL_WINDOW_FOREIGN) \
	F(allow_high_dpi, SDL_WINDOW_ALLOW_HIGHDPI) \
	F(mouse_capture, SDL_WINDOW_MOUSE_CAPTURE) \
	F(always_on_top, SDL_WINDOW_ALWAYS_ON_TOP) \
	F(skip_taskbar, SDL_WINDOW_SKIP_TASKBAR) \
	F(utility, SDL_WINDOW_UTILITY) \
	F(tooltip, SDL_WINDOW_TOOLTIP) \
	F(popup_menu, SDL_WINDOW_POPUP_MENU) \
	F(vulkan, SDL_WINDOW_VULKAN)

static NIF_LIST_TO_FLAGS_FUNCTION(list_to_window_flags, Uint32, WINDOW_FLAGS)
static NIF_FLAGS_TO_LIST_FUNCTION(window_flags_to_list, Uint32, WINDOW_FLAGS)

#define WINDOW_POS_ENUM(E) \
	E(centered, SDL_WINDOWPOS_CENTERED) \
	E(undefined, SDL_WINDOWPOS_UNDEFINED)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_window_pos, int, WINDOW_POS_ENUM)

#define WINDOW_EVENT_ENUM(E) \
	E(shown, SDL_WINDOWEVENT_SHOWN) \
	E(hidden, SDL_WINDOWEVENT_HIDDEN) \
	E(exposed, SDL_WINDOWEVENT_EXPOSED) \
	E(moved, SDL_WINDOWEVENT_MOVED) \
	E(resized, SDL_WINDOWEVENT_RESIZED) \
	E(size_changed, SDL_WINDOWEVENT_SIZE_CHANGED) \
	E(minimized, SDL_WINDOWEVENT_MINIMIZED) \
	E(maximized, SDL_WINDOWEVENT_MAXIMIZED) \
	E(restored, SDL_WINDOWEVENT_RESTORED) \
	E(enter, SDL_WINDOWEVENT_ENTER) \
	E(leave, SDL_WINDOWEVENT_LEAVE) \
	E(focus_gained, SDL_WINDOWEVENT_FOCUS_GAINED) \
	E(focus_lost, SDL_WINDOWEVENT_FOCUS_LOST) \
	E(close, SDL_WINDOWEVENT_CLOSE) \
	E(take_focus, SDL_WINDOWEVENT_TAKE_FOCUS) \
	E(hit_test, SDL_WINDOWEVENT_HIT_TEST)

NIF_ENUM_TO_ATOM_FUNCTION(window_event_to_atom, Uint8, WINDOW_EVENT_ENUM)

#define WINDOW_FULLSCREEN_ENUM(E) \
	E(fullscreen, SDL_WINDOW_FULLSCREEN) \
	E(fullscreen_desktop, SDL_WINDOW_FULLSCREEN_DESKTOP) \
	E(windowed, 0)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_window_fullscreen, Uint32, WINDOW_FULLSCREEN_ENUM)

// create_window

NIF_CALL_HANDLER(thread_create_window)
{
	SDL_Window* window;
	obj_Window* res;
	ERL_NIF_TERM term;

	window = SDL_CreateWindow(args[0], (long)args[1], (long)args[2], (long)args[3], (long)args[4], (long)args[5]);

	enif_free(args[0]);

	if (!window)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Window, window, res, term);

	esdl2_windows_insert(window, res);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(create_window)
{
	unsigned int len;
	char* title;
	int x, y, w, h;
	Uint32 flags = 0;

	if (enif_is_atom(env, argv[1])) {
		BADARG_IF(!atom_to_window_pos(env, argv[1], &x));
	} else {
		BADARG_IF(!enif_get_int(env, argv[1], &x));
	}

	if (enif_is_atom(env, argv[2])) {
		BADARG_IF(!atom_to_window_pos(env, argv[2], &y));
	} else {
		BADARG_IF(!enif_get_int(env, argv[2], &y));
	}

	BADARG_IF(!enif_get_int(env, argv[3], &w));
	BADARG_IF(!enif_get_int(env, argv[4], &h));
	BADARG_IF(!list_to_window_flags(env, argv[5], &flags));

	// Getting the title last to simplify the code due to memory allocation.

	BADARG_IF(!enif_get_list_length(env, argv[0], &len));
	title = (char*)enif_alloc(len + 1);

	if (!enif_get_string(env, argv[0], title, len + 1, ERL_NIF_LATIN1)) {
		enif_free(title);
		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_create_window, 6,
		title, x, y, w, h, flags);
}

// create_window_and_renderer

NIF_CALL_HANDLER(thread_create_window_and_renderer)
{
	SDL_Window* window;
	SDL_Renderer* renderer;
	ERL_NIF_TERM wterm, rterm;

	if (0 != SDL_CreateWindowAndRenderer((long)args[0], (long)args[1], (long)args[2], &window, &renderer))
		return sdl_error_tuple(env);

	NIF_RES_TO_TERM(Window, window, wterm);
	NIF_RES_TO_TERM(Renderer, renderer, rterm);

	return enif_make_tuple3(env,
		atom_ok,
		wterm,
		rterm
	);
}

NIF_FUNCTION(create_window_and_renderer)
{
	int w, h;
	Uint32 flags = 0;

	BADARG_IF(!enif_get_int(env, argv[0], &w));
	BADARG_IF(!enif_get_int(env, argv[1], &h));
	BADARG_IF(!list_to_window_flags(env, argv[2], &flags));

	return nif_thread_call(env, thread_create_window_and_renderer, 3,
		w, h, flags);
}

// get_window_brightness

NIF_CALL_HANDLER(thread_get_window_brightness)
{
	return enif_make_double(env, SDL_GetWindowBrightness(args[0]));
}

NIF_FUNCTION(get_window_brightness)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_brightness, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_display_index

NIF_CALL_HANDLER(thread_get_window_display_index)
{
	int i = SDL_GetWindowDisplayIndex(args[0]);

	if (i < 0)
		return sdl_error_tuple(env);

	return enif_make_tuple2(env,
		atom_ok,
		enif_make_int(env, i)
	);
}

NIF_FUNCTION(get_window_display_index)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_display_index, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_flags

NIF_CALL_HANDLER(thread_get_window_flags)
{
	return window_flags_to_list(env, SDL_GetWindowFlags(args[0]));
}

NIF_FUNCTION(get_window_flags)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_flags, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_grab

NIF_CALL_HANDLER(thread_get_window_grab)
{
	if (SDL_GetWindowGrab(args[0]))
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(get_window_grab)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_grab, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_id

NIF_CALL_HANDLER(thread_get_window_id)
{
	return enif_make_uint(env, SDL_GetWindowID(args[0]));
}

NIF_FUNCTION(get_window_id)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_id, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_maximum_size

NIF_CALL_HANDLER(thread_get_window_maximum_size)
{
	int w, h;

	SDL_GetWindowMaximumSize(args[0], &w, &h);

	return enif_make_tuple2(env,
		enif_make_int(env, w),
		enif_make_int(env, h)
	);
}

NIF_FUNCTION(get_window_maximum_size)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_maximum_size, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_minimum_size

NIF_CALL_HANDLER(thread_get_window_minimum_size)
{
	int w, h;

	SDL_GetWindowMinimumSize(args[0], &w, &h);

	return enif_make_tuple2(env,
		enif_make_int(env, w),
		enif_make_int(env, h)
	);
}

NIF_FUNCTION(get_window_minimum_size)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_minimum_size, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_position

NIF_CALL_HANDLER(thread_get_window_position)
{
	int x, y;

	SDL_GetWindowPosition(args[0], &x, &y);

	return enif_make_tuple2(env,
		enif_make_int(env, x),
		enif_make_int(env, y)
	);
}

NIF_FUNCTION(get_window_position)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_position, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_size

NIF_CALL_HANDLER(thread_get_window_size)
{
	int w, h;

	SDL_GetWindowSize(args[0], &w, &h);

	return enif_make_tuple2(env,
		enif_make_int(env, w),
		enif_make_int(env, h)
	);
}

NIF_FUNCTION(get_window_size)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_size, 1,
		NIF_RES_GET(Window, window_res));
}

// get_window_title

NIF_CALL_HANDLER(thread_get_window_title)
{
	return enif_make_string(env, SDL_GetWindowTitle(args[0]), ERL_NIF_LATIN1);
}

NIF_FUNCTION(get_window_title)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_get_window_title, 1,
		NIF_RES_GET(Window, window_res));
}

// hide_window

NIF_CAST_HANDLER(thread_hide_window)
{
	SDL_HideWindow(args[0]);
}

NIF_FUNCTION(hide_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_hide_window, 1,
		NIF_RES_GET(Window, window_res));
}

// maximize_window

NIF_CAST_HANDLER(thread_maximize_window)
{
	SDL_MaximizeWindow(args[0]);
}

NIF_FUNCTION(maximize_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_maximize_window, 1,
		NIF_RES_GET(Window, window_res));
}

// minimize_window

NIF_CAST_HANDLER(thread_minimize_window)
{
	SDL_MinimizeWindow(args[0]);
}

NIF_FUNCTION(minimize_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_minimize_window, 1,
		NIF_RES_GET(Window, window_res));
}

// raise_window

NIF_CAST_HANDLER(thread_raise_window)
{
	SDL_RaiseWindow(args[0]);
}

NIF_FUNCTION(raise_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_raise_window, 1,
		NIF_RES_GET(Window, window_res));
}

// restore_window

NIF_CAST_HANDLER(thread_restore_window)
{
	SDL_RestoreWindow(args[0]);
}

NIF_FUNCTION(restore_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_restore_window, 1,
		NIF_RES_GET(Window, window_res));
}

// set_window_bordered

NIF_CAST_HANDLER(thread_set_window_bordered)
{
	SDL_SetWindowBordered(args[0], (long)args[1]);
}

NIF_FUNCTION(set_window_bordered)
{
	void* window_res;
	SDL_bool b;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!atom_to_bool(env, argv[1], &b));

	return nif_thread_cast(env, thread_set_window_bordered, 2,
		NIF_RES_GET(Window, window_res), b);
}

// set_window_brightness

NIF_CALL_HANDLER(thread_set_window_brightness)
{
	int ret = SDL_SetWindowBrightness(args[0], *((double*)args[1]));

	enif_free(args[1]);

	if (ret != 0)
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(set_window_brightness)
{
	void* window_res;
	double *brightnessPtr;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	brightnessPtr = (double*)enif_alloc(sizeof(double));
	if (!enif_get_double(env, argv[1], brightnessPtr)) {
		enif_free(brightnessPtr);
		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_set_window_brightness, 2,
		NIF_RES_GET(Window, window_res), brightnessPtr);
}

// set_window_fullscreen

NIF_CALL_HANDLER(thread_set_window_fullscreen)
{
	if (SDL_SetWindowFullscreen(args[0], (long)args[1]))
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(set_window_fullscreen)
{
	void* window_res;
	Uint32 flags;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!atom_to_window_fullscreen(env, argv[1], &flags));

	return nif_thread_call(env, thread_set_window_fullscreen, 2,
		NIF_RES_GET(Window, window_res), flags);
}

// set_window_grab

NIF_CAST_HANDLER(thread_set_window_grab)
{
	SDL_SetWindowGrab(args[0], (long)args[1]);
}

NIF_FUNCTION(set_window_grab)
{
	void* window_res;
	SDL_bool b;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!atom_to_bool(env, argv[1], &b));

	return nif_thread_cast(env, thread_set_window_grab, 2,
		NIF_RES_GET(Window, window_res), b);
}

// set_window_icon
//
// We use a call here because we need the surface to exist until this call
// succeeds. If we didn't, a race condition might happen where the surface
// is GC before it is used in the main thread.

NIF_CALL_HANDLER(thread_set_window_icon)
{
	SDL_SetWindowIcon(args[0], args[1]);

	return atom_ok;
}

NIF_FUNCTION(set_window_icon)
{
	void* window_res;
	void* surface_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_resource(env, argv[1], res_Surface, &surface_res));

	return nif_thread_call(env, thread_set_window_icon, 2,
		NIF_RES_GET(Window, window_res), NIF_RES_GET(Surface, surface_res));
}

// set_window_maximum_size

NIF_CAST_HANDLER(thread_set_window_maximum_size)
{
	SDL_SetWindowMaximumSize(args[0], (long)args[1], (long)args[2]);
}

NIF_FUNCTION(set_window_maximum_size)
{
	void* window_res;
	int w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_int(env, argv[1], &w));
	BADARG_IF(!enif_get_int(env, argv[2], &h));

	return nif_thread_cast(env, thread_set_window_maximum_size, 3,
		NIF_RES_GET(Window, window_res), w, h);
}

// set_window_minimum_size

NIF_CAST_HANDLER(thread_set_window_minimum_size)
{
	SDL_SetWindowMinimumSize(args[0], (long)args[1], (long)args[2]);
}

NIF_FUNCTION(set_window_minimum_size)
{
	void* window_res;
	int w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_int(env, argv[1], &w));
	BADARG_IF(!enif_get_int(env, argv[2], &h));

	return nif_thread_cast(env, thread_set_window_minimum_size, 3,
		NIF_RES_GET(Window, window_res), w, h);
}

// set_window_position

NIF_CAST_HANDLER(thread_set_window_position)
{
	SDL_SetWindowPosition(args[0], (long)args[1], (long)args[2]);
}

NIF_FUNCTION(set_window_position)
{
	void* window_res;
	int x, y;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_int(env, argv[1], &x));
	BADARG_IF(!enif_get_int(env, argv[2], &y));

	return nif_thread_cast(env, thread_set_window_position, 3,
		NIF_RES_GET(Window, window_res), x, y);
}

// set_window_size

NIF_CAST_HANDLER(thread_set_window_size)
{
	SDL_SetWindowSize(args[0], (long)args[1], (long)args[2]);
}

NIF_FUNCTION(set_window_size)
{
	void* window_res;
	int w, h;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));
	BADARG_IF(!enif_get_int(env, argv[1], &w));
	BADARG_IF(!enif_get_int(env, argv[2], &h));

	return nif_thread_cast(env, thread_set_window_size, 3,
		NIF_RES_GET(Window, window_res), w, h);
}

// set_window_title

NIF_CAST_HANDLER(thread_set_window_title)
{
	SDL_SetWindowTitle(args[0], args[1]);

	enif_free(args[1]);
}

NIF_FUNCTION(set_window_title)
{
	void* window_res;
	unsigned int len;
	char* title;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	BADARG_IF(!enif_get_list_length(env, argv[1], &len));
	title = (char*)enif_alloc(len + 1);

	if (!enif_get_string(env, argv[1], title, len + 1, ERL_NIF_LATIN1)) {
		enif_free(title);
		return enif_make_badarg(env);
	}

	return nif_thread_cast(env, thread_set_window_title, 2,
		NIF_RES_GET(Window, window_res), title);
}

// show_window

NIF_CAST_HANDLER(thread_show_window)
{
	SDL_ShowWindow(args[0]);
}

NIF_FUNCTION(show_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_show_window, 1,
		NIF_RES_GET(Window, window_res));
}

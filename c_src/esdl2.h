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

#ifndef __ESDL2_H__
#define __ESDL2_H__

#include "SDL.h"

// List of atoms used by this NIF.

#define NIF_ATOMS(A) \
	A(allow_high_dpi) \
	A(borderless) \
	A(button) \
	A(caps) \
	A(charged) \
	A(charging) \
	A(close) \
	A(data) \
	A(enter) \
	A(error) \
	A(event) \
	A(exposed) \
	A(false) \
	A(focus_gained) \
	A(focus_lost) \
	A(foreign) \
	A(fullscreen) \
	A(fullscreen_desktop) \
	A(h) \
	A(hidden) \
	A(input_focus) \
	A(input_grabbed) \
	A(key_down) \
	A(key_up) \
	A(leave) \
	A(left) \
	A(left_alt) \
	A(left_ctrl) \
	A(left_gui) \
	A(left_shift) \
	A(ok) \
	A(maximized) \
	A(middle) \
	A(minimized) \
	A(mod) \
	A(mode) \
	A(mouse_down) \
	A(mouse_focus) \
	A(mouse_motion) \
	A(mouse_up) \
	A(mouse_wheel) \
	A(moved) \
	A(no_battery) \
	A(num) \
	A(on_battery) \
	A(opengl) \
	A(quit) \
	A(repeat) \
	A(resizable) \
	A(resized) \
	A(restored) \
	A(right) \
	A(right_alt) \
	A(right_ctrl) \
	A(right_gui) \
	A(right_shift) \
	A(scancode) \
	A(shown) \
	A(size_changed) \
	A(state) \
	A(sym) \
	A(touch) \
	A(true) \
	A(timestamp) \
	A(type) \
	A(undefined) \
	A(unknown) \
	A(w) \
	A(which) \
	A(window) \
	A(window_id) \
	A(x) \
	A(x1) \
	A(x2) \
	A(xrel) \
	A(y) \
	A(yrel) \
	A(_nif_thread_ret_)

// List of resources used by this NIF.

#define NIF_RES_TYPE(r) SDL_ ## r
#define NIF_RESOURCES(R) \
	R(Renderer) \
	R(Surface) \
	R(Texture) \
	R(Window)

// List of functions defined in this NIF.

#define NIF_FUNCTION_NAME(f) esdl2_ ## f
#define NIF_FUNCTIONS(F) \
	/* sdl */ \
	F(init, 1) \
	F(init_subsystem, 1) \
	F(quit, 0) \
	F(quit_subsystem, 1) \
	F(set_main_ready, 0) \
	F(was_init, 1) \
	/* sdl_clipboard */ \
	F(get_clipboard_text, 0) \
	F(has_clipboard_text, 0) \
	F(set_clipboard_text, 1) \
	/* sdl_cpu_info */ \
	F(get_cpu_cache_line_size, 0) \
	F(get_cpu_count, 0) \
	F(get_system_ram, 0) \
	F(has_3dnow, 0) \
	F(has_avx, 0) \
	F(has_altivec, 0) \
	F(has_mmx, 0) \
	F(has_rdtsc, 0) \
	F(has_sse, 0) \
	F(has_sse2, 0) \
	F(has_sse3, 0) \
	F(has_sse41, 0) \
	F(has_sse42, 0) \
	/* sdl_events */ \
	F(poll_event, 0) \
	/* sdl_filesystem */ \
	F(get_base_path, 0) \
	F(get_pref_path, 2) \
	/* sdl_power */ \
	F(get_power_info, 0) \
	/* sdl_renderer */ \
	F(create_renderer, 3) \
	F(render_clear, 1) \
	F(render_copy, 4) \
	F(render_present, 1) \
	F(render_set_logical_size, 3) \
	F(set_render_draw_color, 5) \
	/* sdl_surface */ \
	F(img_load, 1) \
	/* sdl_texture */ \
	F(create_texture_from_surface, 2) \
	/* sdl_version */ \
	F(get_version, 0) \
	F(get_revision, 0) \
	/* sdl_window */ \
	F(create_window, 6) \
	F(create_window_and_renderer, 3) \
	F(get_window_brightness, 1) \
	F(get_window_display_index, 1) \
	F(get_window_flags, 1) \
	F(get_window_grab, 1) \
	F(get_window_id, 1) \
	F(get_window_maximum_size, 1) \
	F(get_window_minimum_size, 1) \
	F(get_window_position, 1) \
	F(get_window_size, 1) \
	F(get_window_title, 1) \
	F(hide_window, 1) \
	F(maximize_window, 1) \
	F(minimize_window, 1) \
	F(raise_window, 1) \

// Generated declarations for the NIF.

#include "nif_helpers.h"

NIF_ATOMS(NIF_ATOM_H_DECL)
NIF_RESOURCES(NIF_RES_H_DECL)
NIF_FUNCTIONS(NIF_FUNCTION_H_DECL)

// --

#define sdl_error_tuple(env) \
	enif_make_tuple2(env, \
		atom_error, \
		enif_make_string(env, SDL_GetError(), ERL_NIF_LATIN1) \
	);

#endif

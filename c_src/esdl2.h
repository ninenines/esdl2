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

#ifndef __ESDL2_H__
#define __ESDL2_H__

#include "SDL.h"
#include "SDL_ttf.h"

// List of atoms used by this NIF.

#define NIF_ATOMS(A) \
	A(a) \
	A(abgr1555) \
	A(abgr32) \
	A(abgr4444) \
	A(abgr8888) \
	A(accelerated) \
	A(add) \
	A(allow_high_dpi) \
	A(always_on_top) \
	A(app_did_enter_background) \
	A(app_did_enter_foreground) \
	A(app_low_memory) \
	A(app_terminating) \
	A(app_will_enter_background) \
	A(app_will_enter_foreground) \
	A(argb1555) \
	A(argb2101010) \
	A(argb32) \
	A(argb4444) \
	A(argb8888) \
	A(arrow) \
	A(audio) \
	A(audio_device_added) \
	A(audio_device_removed) \
	A(b) \
	A(bgr24) \
	A(bgr555) \
	A(bgr565) \
	A(bgr888) \
	A(bgra32) \
	A(bgra4444) \
	A(bgra5551) \
	A(bgra8888) \
	A(bgrx8888) \
	A(blend) \
	A(bold) \
	A(borderless) \
	A(bottom) \
	A(bottom_left) \
	A(bottom_right) \
	A(button) \
	A(callback) \
	A(caps) \
	A(centered) \
	A(charged) \
	A(charging) \
	A(clicks) \
	A(clipboard_update) \
	A(close) \
	A(controller_axis_motion) \
	A(controller_button_down) \
	A(controller_button_up) \
	A(controller_device_added) \
	A(controller_device_remapped) \
	A(controller_device_removed) \
	A(crosshair) \
	A(data1) \
	A(data2) \
	A(diagonal) \
	A(direction) \
	A(dollar_gesture) \
	A(dollar_record) \
	A(draggable) \
	A(drop_begin) \
	A(drop_complete) \
	A(drop_file) \
	A(drop_text) \
	A(dst_alpha) \
	A(dst_color) \
	A(enter) \
	A(error) \
	A(event) \
	A(events) \
	A(everything) \
	A(exposed) \
	A(false) \
	A(finger_down) \
	A(finger_motion) \
	A(finger_up) \
	A(first) \
	A(flags) \
	A(flipped) \
	A(focus_gained) \
	A(focus_lost) \
	A(foreign) \
	A(format) \
	A(fullscreen) \
	A(fullscreen_desktop) \
	A(g) \
	A(game_controller) \
	A(get) \
	A(h) \
	A(hand) \
	A(haptic) \
	A(hidden) \
	A(hit_test) \
	A(horizontal) \
	A(ibeam) \
	A(index1lsb) \
	A(index1msb) \
	A(index4lsb) \
	A(index4msb) \
	A(index8) \
	A(input_focus) \
	A(input_grabbed) \
	A(invalid) \
	A(italic) \
	A(iyuv) \
	A(joy_axis_motion) \
	A(joy_ball_motion) \
	A(joy_button_down) \
	A(joy_button_up) \
	A(joy_device_added) \
	A(joy_device_removed) \
	A(joy_hat_motion) \
	A(joystick) \
	A(key_down) \
	A(key_up) \
	A(keymap_changed) \
	A(last) \
	A(leave) \
	A(left) \
	A(left_alt) \
	A(left_ctrl) \
	A(left_gui) \
	A(left_shift) \
	A(light) \
	A(max_texture_height) \
	A(max_texture_width) \
	A(maximized) \
	A(maximum) \
	A(middle) \
	A(minimized) \
	A(minimum) \
	A(mod) \
	A(mode) \
	A(mono) \
	A(mouse_capture) \
	A(mouse_down) \
	A(mouse_focus) \
	A(mouse_motion) \
	A(mouse_up) \
	A(mouse_wheel) \
	A(moved) \
	A(multi_gesture) \
	A(name) \
	A(no) \
	A(no_battery) \
	A(none) \
	A(normal) \
	A(num) \
	A(nv12) \
	A(nv21) \
	A(ok) \
	A(on_battery) \
	A(one) \
	A(one_minus_dst_alpha) \
	A(one_minus_dst_color) \
	A(one_minus_src_alpha) \
	A(one_minus_src_color) \
	A(opengl) \
	A(peek) \
	A(popup_menu) \
	A(present_vsync) \
	A(pressed) \
	A(quit) \
	A(r) \
	A(refresh_rate) \
	A(released) \
	A(render_device_reset) \
	A(render_targets_reset) \
	A(repeat) \
	A(resizable) \
	A(resized) \
	A(restored) \
	A(rev_substract) \
	A(rgb24) \
	A(rgb332) \
	A(rgb444) \
	A(rgb555) \
	A(rgb565) \
	A(rgb888) \
	A(rgba32) \
	A(rgba4444) \
	A(rgba5551) \
	A(rgba8888) \
	A(rgbx8888) \
	A(right) \
	A(right_alt) \
	A(right_ctrl) \
	A(right_gui) \
	A(right_shift) \
	A(scancode) \
	A(set_window_hit_test_result) \
	A(shown) \
	A(size_all) \
	A(size_changed) \
	A(size_nesw) \
	A(size_ns) \
	A(size_nwse) \
	A(size_we) \
	A(skip_taskbar) \
	A(software) \
	A(src_alpha) \
	A(src_color) \
	A(state) \
	A(strikethrough) \
	A(substract) \
	A(sym) \
	A(syswm) \
	A(take_focus) \
	A(target_texture) \
	A(text_editing) \
	A(text_input) \
	A(texture_formats) \
	A(timer) \
	A(timestamp) \
	A(tooltip) \
	A(top) \
	A(top_left) \
	A(top_right) \
	A(touch) \
	A(true) \
	A(type) \
	A(undefined) \
	A(underline) \
	A(unknown) \
	A(utility) \
	A(uyvy) \
	A(vertical) \
	A(video) \
	A(vulkan) \
	A(w) \
	A(wait) \
	A(wait_arrow) \
	A(which) \
	A(window) \
	A(window_id) \
	A(windowed) \
	A(x) \
	A(x1) \
	A(x2) \
	A(xrel) \
	A(y) \
	A(yrel) \
	A(yuy2) \
	A(yv12) \
	A(yvyu) \
	A(zero) \
	A(_nif_thread_ret_)

// List of resources used by this NIF.

#define SDL_Font TTF_Font

#define NIF_RES_TYPE(r) SDL_ ## r
#define NIF_RESOURCES(R) \
	R(Cursor) \
	R(Font) \
	R(GLContext) \
	R(Renderer) \
	R(Surface) \
	R(Texture) \
	R(Window)

// List of functions defined in this NIF.

#define NIF_FUNCTION_NAME(f) esdl2_ ## f
#define NIF_FUNCTIONS(F) \
	/* internal */ \
	F(register_callback_process, 0) \
	/* sdl */ \
	F(init, 1) \
	F(init_subsystem, 1) \
	F(quit, 0) \
	F(quit_subsystem, 1) \
	F(set_main_ready, 0) \
	F(was_init, 1) \
	/* sdl_blendmode */ \
	F(compose_custom_blend_mode, 6) \
	/* sdl_clipboard */ \
	F(get_clipboard_text, 0) \
	F(has_clipboard_text, 0) \
	F(set_clipboard_text, 1) \
	/* sdl_cpu_info */ \
	F(get_cpu_cache_line_size, 0) \
	F(get_cpu_count, 0) \
	F(get_system_ram, 0) \
	F(has_3dnow, 0) \
	F(has_altivec, 0) \
	F(has_avx, 0) \
	F(has_avx2, 0) \
	F(has_mmx, 0) \
	F(has_neon, 0) \
	F(has_rdtsc, 0) \
	F(has_sse, 0) \
	F(has_sse2, 0) \
	F(has_sse3, 0) \
	F(has_sse41, 0) \
	F(has_sse42, 0) \
	/* sdl_cursor */ \
	F(create_cursor, 6) \
	F(create_color_cursor, 3) \
	F(create_system_cursor, 1) \
	F(get_cursor, 0) \
	F(get_default_cursor, 0) \
	F(set_cursor, 1) \
	F(show_cursor, 1) \
	/* sdl_events */ \
	F(flush_event, 1) \
	F(flush_events, 2) \
	F(has_event, 1) \
	F(has_events, 2) \
	F(peep_events, 4) \
	F(poll_event, 0) \
	F(pump_events, 0) \
	/* sdl_filesystem */ \
	F(get_base_path, 0) \
	F(get_pref_path, 2) \
	/* sdl_gl */ \
	F(gl_create_context, 1) \
	F(gl_swap_window, 1) \
	/* sdl_hints */ \
	F(add_hint_callback, 3) \
	/* sdl_keyboard */ \
	F(get_key_from_name, 1) \
	F(get_key_from_scancode, 1) \
	F(get_key_name, 1) \
	F(get_keyboard_focus, 0) \
	F(get_keyboard_state, 0) \
	F(get_mod_state, 0) \
	F(get_scancode_from_key, 1) \
	F(get_scancode_from_name, 1) \
	F(get_scancode_name, 1) \
	F(has_screen_keyboard_support, 0) \
	F(is_screen_keyboard_shown, 1) \
	F(is_text_input_active, 0) \
	F(set_mod_state, 1) \
	F(set_text_input_rect, 1) \
	F(start_text_input, 0) \
	F(stop_text_input, 0) \
	/* sdl_mouse */ \
	F(capture_mouse, 1) \
	F(get_global_mouse_state, 0) \
	F(get_mouse_focus, 0) \
	F(get_mouse_state, 0) \
	F(get_relative_mouse_mode, 0) \
	F(get_relative_mouse_state, 0) \
	F(set_relative_mouse_mode, 1) \
	F(warp_mouse_global, 2) \
	F(warp_mouse_in_window, 3) \
	/* sdl_platform */ \
	F(get_platform, 0) \
	/* sdl_power */ \
	F(get_power_info, 0) \
	/* sdl_rect */ \
	F(enclose_points, 2) \
	F(has_intersection, 2) \
	F(intersect_rect, 2) \
	F(intersect_rect_and_line, 5) \
	F(union_rect, 2) \
	/* sdl_renderer */ \
	F(create_renderer, 3) \
	F(get_num_render_drivers, 0) \
	F(get_render_draw_blend_mode, 1) \
	F(get_render_draw_color, 1) \
	F(get_render_driver_info, 1) \
	F(get_render_output_size, 1) \
	F(get_renderer, 1) \
	F(get_renderer_info, 1) \
	F(render_clear, 1) \
	F(render_copy, 4) \
	F(render_copy_ex, 7) \
	F(render_draw_line, 5) \
	F(render_draw_lines, 2) \
	F(render_draw_point, 3) \
	F(render_draw_points, 2) \
	F(render_draw_rect, 5) \
	F(render_draw_rects, 2) \
	F(render_fill_rect, 5) \
	F(render_fill_rects, 2) \
	F(render_get_clip_rect, 1) \
	F(render_get_integer_scale, 1) \
	F(render_get_logical_size, 1) \
	F(render_get_scale, 1) \
	F(render_get_viewport, 1) \
	F(render_present, 1) \
	F(render_set_clip_rect, 5) \
	F(render_set_integer_scale, 2) \
	F(render_set_logical_size, 3) \
	F(render_set_scale, 3) \
	F(render_set_viewport, 5) \
	F(render_target_supported, 1) \
	F(set_render_draw_blend_mode, 2) \
	F(set_render_draw_color, 5) \
	/* sdl_stdinc */ \
	F(get_num_allocations, 0) \
	/* sdl_surface */ \
	F(get_surface_dimensions, 1) \
	F(img_load, 1) \
	/* sdl_texture */ \
	F(create_texture_from_surface, 2) \
	F(get_texture_alpha_mod, 1) \
	F(get_texture_blend_mode, 1) \
	F(get_texture_color_mod, 1) \
	F(set_texture_alpha_mod, 2) \
	F(set_texture_blend_mode, 2) \
	F(set_texture_color_mod, 4) \
	/* sdl_ttf */ \
	F(ttf_font_ascent, 1) \
	F(ttf_font_descent, 1) \
	F(ttf_font_face_family_name, 1) \
	F(ttf_font_face_is_fixed_width, 1) \
	F(ttf_font_face_style_name, 1) \
	F(ttf_font_faces, 1) \
	F(ttf_font_height, 1) \
	F(ttf_font_line_skip, 1) \
	F(ttf_get_font_hinting, 1) \
	F(ttf_get_font_kerning, 1) \
	F(ttf_get_font_kerning_size_glyphs, 3) \
	F(ttf_get_font_outline, 1) \
	F(ttf_get_font_style, 1) \
	F(ttf_glyph_is_provided, 2) \
	F(ttf_glyph_metrics, 2) \
	F(ttf_init, 0) \
	F(ttf_open_font, 2) \
	F(ttf_open_font_index, 3) \
	F(ttf_quit, 0) \
	F(ttf_render_utf8_blended, 3) \
	F(ttf_render_utf8_blended_wrapped, 4) \
	F(ttf_render_utf8_shaded, 4) \
	F(ttf_render_utf8_solid, 3) \
	F(ttf_set_font_hinting, 2) \
	F(ttf_set_font_kerning, 2) \
	F(ttf_set_font_outline, 2) \
	F(ttf_set_font_style, 2) \
	F(ttf_size_utf8, 2) \
	F(ttf_was_init, 0) \
	/* sdl_version */ \
	F(get_version, 0) \
	F(get_revision, 0) \
	/* sdl_video */ \
	F(disable_screensaver, 0) \
	F(enable_screensaver, 0) \
	F(get_closest_display_mode, 2) \
	F(get_current_display_mode, 1) \
	F(get_current_video_driver, 0) \
	F(get_desktop_display_mode, 1) \
	F(get_display_bounds, 1) \
	F(get_display_dpi, 1) \
	F(get_display_mode, 2) \
	F(get_display_name, 1) \
	F(get_display_usable_bounds, 1) \
	F(get_num_display_modes, 1) \
	F(get_num_video_displays, 0) \
	F(get_num_video_drivers, 0) \
	F(get_video_driver, 1) \
	F(is_screensaver_enabled, 0) \
	F(video_init, 1) \
	F(video_quit, 0) \
	/* sdl_window */ \
	F(create_window, 6) \
	F(create_window_and_renderer, 3) \
	F(get_grabbed_window, 0) \
	F(get_window_borders_size, 1) \
	F(get_window_brightness, 1) \
	F(get_window_display_index, 1) \
	F(get_window_display_mode, 1) \
	F(get_window_flags, 1) \
	F(get_window_from_id, 1) \
	F(get_window_gamma_ramp, 1) \
	F(get_window_grab, 1) \
	F(get_window_id, 1) \
	F(get_window_maximum_size, 1) \
	F(get_window_minimum_size, 1) \
	F(get_window_opacity, 1) \
	F(get_window_pixel_format, 1) \
	F(get_window_position, 1) \
	F(get_window_size, 1) \
	F(get_window_title, 1) \
	F(hide_window, 1) \
	F(maximize_window, 1) \
	F(minimize_window, 1) \
	F(raise_window, 1) \
	F(restore_window, 1) \
	F(set_window_bordered, 2) \
	F(set_window_brightness, 2) \
	F(set_window_display_mode, 2) \
	F(set_window_fullscreen, 2) \
	F(set_window_gamma_ramp, 4) \
	F(set_window_grab, 2) \
	F(set_window_hit_test, 3) \
	F(set_window_hit_test_remove, 1) \
	F(set_window_hit_test_result, 2) \
	F(set_window_icon, 2) \
	F(set_window_input_focus, 1) \
	F(set_window_maximum_size, 3) \
	F(set_window_minimum_size, 3) \
	F(set_window_modal_for, 2) \
	F(set_window_opacity, 2) \
	F(set_window_position, 3) \
	F(set_window_resizable, 2) \
	F(set_window_size, 3) \
	F(set_window_title, 2) \
	F(show_window, 1)

// Generated declarations for the NIF.

#include "nif_helpers.h"

NIF_ATOMS(NIF_ATOM_H_DECL)
NIF_RESOURCES(NIF_RES_H_DECL)
NIF_FUNCTIONS(NIF_FUNCTION_H_DECL)

// Utility functions used across different files.

NIF_ATOM_TO_ENUM_FUNCTION_DECL(atom_to_blend_mode, SDL_BlendMode)
NIF_ATOM_TO_ENUM_FUNCTION_DECL(atom_to_bool, SDL_bool)
NIF_ATOM_TO_ENUM_FUNCTION_DECL(atom_to_pixel_format, Uint32)
NIF_ENUM_TO_ATOM_FUNCTION_DECL(blend_mode_to_atom, SDL_BlendMode)
NIF_ENUM_TO_ATOM_FUNCTION_DECL(button_to_atom, Uint8)
NIF_ENUM_TO_ATOM_FUNCTION_DECL(mousewheel_direction_to_atom, Uint32)
NIF_ENUM_TO_ATOM_FUNCTION_DECL(pixel_format_to_atom, Uint32)
NIF_ENUM_TO_ATOM_FUNCTION_DECL(window_event_to_atom, Uint8)

NIF_LIST_TO_FLAGS_FUNCTION_DECL(keymod_list_to_flags, Uint16)
NIF_FLAGS_TO_LIST_FUNCTION_DECL(keymod_flags_to_list, Uint16)

int map_to_color(ErlNifEnv*, ERL_NIF_TERM, SDL_Color*);
int map_to_point(ErlNifEnv*, ERL_NIF_TERM, SDL_Point*);
ERL_NIF_TERM point_to_map(ErlNifEnv*, const SDL_Point*);
int map_to_rect(ErlNifEnv*, ERL_NIF_TERM, SDL_Rect*);
ERL_NIF_TERM rect_to_map(ErlNifEnv*, const SDL_Rect*);

ERL_NIF_TERM display_mode_to_map(ErlNifEnv*, SDL_DisplayMode*);
ERL_NIF_TERM mouse_state_to_list(ErlNifEnv*, Uint32);

// --

void esdl2_windows_init(void);
void esdl2_windows_insert(SDL_Window*, obj_Window*);
ERL_NIF_TERM esdl2_windows_find(ErlNifEnv*, SDL_Window*);
void esdl2_windows_remove(SDL_Window*);
void esdl2_windows_free(void);

void esdl2_renderers_init(void);
void esdl2_renderers_insert(SDL_Renderer*, obj_Renderer*, obj_Window*);
ERL_NIF_TERM esdl2_renderers_find(ErlNifEnv*, SDL_Renderer*);
void esdl2_renderers_remove(SDL_Renderer*);
void esdl2_renderers_free(void);

void esdl2_cursors_init(void);
void esdl2_cursors_insert(SDL_Cursor*, obj_Cursor*);
ERL_NIF_TERM esdl2_cursors_find(ErlNifEnv*, SDL_Cursor*);
void esdl2_cursors_remove(SDL_Cursor*);
void esdl2_cursors_free(void);

ErlNifPid* get_callback_process(void);

#define sdl_error_tuple(env) \
	enif_make_tuple2(env, \
		atom_error, \
		enif_make_string(env, SDL_GetError(), ERL_NIF_LATIN1) \
	);

#endif

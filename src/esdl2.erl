%% Copyright (c) 2014-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(esdl2).

%% internal
-export([register_callback_process/0]).

%% sdl
-export([init/1]).
-export([init_subsystem/1]).
-export([quit/0]).
-export([quit_subsystem/1]).
-export([set_main_ready/0]).
-export([was_init/1]).

%% sdl_blendmode
-export([compose_custom_blend_mode/6]).

%% sdl_clipboard
-export([get_clipboard_text/0]).
-export([has_clipboard_text/0]).
-export([set_clipboard_text/1]).

%% sdl_cpu_info
-export([get_cpu_cache_line_size/0]).
-export([get_cpu_count/0]).
-export([get_system_ram/0]).
-export([has_3dnow/0]).
-export([has_altivec/0]).
-export([has_avx/0]).
-export([has_avx2/0]).
-export([has_mmx/0]).
-export([has_neon/0]).
-export([has_rdtsc/0]).
-export([has_sse/0]).
-export([has_sse2/0]).
-export([has_sse3/0]).
-export([has_sse41/0]).
-export([has_sse42/0]).

%% sdl_cursor
-export([create_cursor/6]).
-export([create_color_cursor/3]).
-export([create_system_cursor/1]).
-export([get_cursor/0]).
-export([get_default_cursor/0]).
-export([set_cursor/1]).
-export([show_cursor/1]).

%% sdl_events
-export([flush_event/1]).
-export([flush_events/2]).
-export([has_event/1]).
-export([has_events/2]).
-export([peep_events/4]).
-export([poll_event/0]).
-export([pump_events/0]).

%% sdl_filesystem
-export([get_base_path/0]).
-export([get_pref_path/2]).

%% sdl_gl
-export([gl_create_context/1]).
-export([gl_swap_window/1]).

%% sdl_hints
-export([add_hint_callback/3]).

%% sdl_keyboard
-export([get_key_from_name/1]).
-export([get_key_from_scancode/1]).
-export([get_key_name/1]).
-export([get_keyboard_focus/0]).
-export([get_keyboard_state/0]).
-export([get_mod_state/0]).
-export([get_scancode_from_key/1]).
-export([get_scancode_from_name/1]).
-export([get_scancode_name/1]).
-export([has_screen_keyboard_support/0]).
-export([is_screen_keyboard_shown/1]).
-export([is_text_input_active/0]).
-export([set_mod_state/1]).
-export([set_text_input_rect/1]).
-export([start_text_input/0]).
-export([stop_text_input/0]).

%% sdl_mouse
-export([capture_mouse/1]).
-export([get_global_mouse_state/0]).
-export([get_mouse_focus/0]).
-export([get_mouse_state/0]).
-export([get_relative_mouse_mode/0]).
-export([get_relative_mouse_state/0]).
-export([set_relative_mouse_mode/1]).
-export([warp_mouse_global/2]).
-export([warp_mouse_in_window/3]).

%% sdl_platform
-export([get_platform/0]).

%% sdl_power
-export([get_power_info/0]).

%% sdl_rect
-export([enclose_points/2]).
-export([has_intersection/2]).
-export([intersect_rect/2]).
-export([intersect_rect_and_line/5]).
-export([union_rect/2]).

%% sdl_renderer
-export([create_renderer/3]).
-export([get_num_render_drivers/0]).
-export([get_render_draw_blend_mode/1]).
-export([get_render_draw_color/1]).
-export([get_render_driver_info/1]).
-export([get_render_output_size/1]).
-export([get_renderer/1]).
-export([get_renderer_info/1]).
-export([render_clear/1]).
-export([render_copy/4]).
-export([render_copy_ex/7]).
-export([render_draw_line/5]).
-export([render_draw_lines/2]).
-export([render_draw_point/3]).
-export([render_draw_points/2]).
-export([render_draw_rect/5]).
-export([render_draw_rects/2]).
-export([render_fill_rect/5]).
-export([render_fill_rects/2]).
-export([render_get_clip_rect/1]).
-export([render_get_integer_scale/1]).
-export([render_get_logical_size/1]).
-export([render_get_scale/1]).
-export([render_get_viewport/1]).
-export([render_present/1]).
-export([render_set_clip_rect/5]).
-export([render_set_integer_scale/2]).
-export([render_set_logical_size/3]).
-export([render_set_scale/3]).
-export([render_set_viewport/5]).
-export([render_target_supported/1]).
-export([set_render_draw_blend_mode/2]).
-export([set_render_draw_color/5]).

%% sdl_stdinc
-export([get_num_allocations/0]).

%% sdl_surface
-export([get_surface_dimensions/1]).
-export([img_load/1]).

%% sdl_texture
-export([create_texture_from_surface/2]).
-export([get_texture_alpha_mod/1]).
-export([get_texture_blend_mode/1]).
-export([get_texture_color_mod/1]).
-export([set_texture_alpha_mod/2]).
-export([set_texture_blend_mode/2]).
-export([set_texture_color_mod/4]).

%% sdl_ttf
-export([ttf_font_ascent/1]).
-export([ttf_font_descent/1]).
-export([ttf_font_face_family_name/1]).
-export([ttf_font_face_is_fixed_width/1]).
-export([ttf_font_face_style_name/1]).
-export([ttf_font_faces/1]).
-export([ttf_font_height/1]).
-export([ttf_font_line_skip/1]).
-export([ttf_get_font_hinting/1]).
-export([ttf_get_font_kerning/1]).
-export([ttf_get_font_kerning_size_glyphs/3]).
-export([ttf_get_font_outline/1]).
-export([ttf_get_font_style/1]).
-export([ttf_glyph_is_provided/2]).
-export([ttf_glyph_metrics/2]).
-export([ttf_init/0]).
-export([ttf_open_font/2]).
-export([ttf_open_font_index/3]).
-export([ttf_quit/0]).
-export([ttf_render_utf8_blended/3]).
-export([ttf_render_utf8_blended_wrapped/4]).
-export([ttf_render_utf8_shaded/4]).
-export([ttf_render_utf8_solid/3]).
-export([ttf_set_font_hinting/2]).
-export([ttf_set_font_kerning/2]).
-export([ttf_set_font_outline/2]).
-export([ttf_set_font_style/2]).
-export([ttf_size_utf8/2]).
-export([ttf_was_init/0]).

%% sdl_version
-export([get_version/0]).
-export([get_revision/0]).

%% sdl_video
-export([disable_screensaver/0]).
-export([enable_screensaver/0]).
-export([get_closest_display_mode/2]).
-export([get_current_display_mode/1]).
-export([get_current_video_driver/0]).
-export([get_desktop_display_mode/1]).
-export([get_display_bounds/1]).
-export([get_display_dpi/1]).
-export([get_display_mode/2]).
-export([get_display_name/1]).
-export([get_display_usable_bounds/1]).
-export([get_num_display_modes/1]).
-export([get_num_video_displays/0]).
-export([get_num_video_drivers/0]).
-export([get_video_driver/1]).
-export([is_screensaver_enabled/0]).
-export([video_init/1]).
-export([video_quit/0]).

%% sdl_window
-export([create_window/6]).
-export([create_window_and_renderer/3]).
-export([get_grabbed_window/0]).
-export([get_window_borders_size/1]).
-export([get_window_brightness/1]).
-export([get_window_display_index/1]).
-export([get_window_display_mode/1]).
-export([get_window_flags/1]).
-export([get_window_from_id/1]).
-export([get_window_gamma_ramp/1]).
-export([get_window_grab/1]).
-export([get_window_id/1]).
-export([get_window_maximum_size/1]).
-export([get_window_minimum_size/1]).
-export([get_window_opacity/1]).
-export([get_window_pixel_format/1]).
-export([get_window_position/1]).
-export([get_window_size/1]).
-export([get_window_title/1]).
-export([hide_window/1]).
-export([maximize_window/1]).
-export([minimize_window/1]).
-export([raise_window/1]).
-export([restore_window/1]).
-export([set_window_bordered/2]).
-export([set_window_brightness/2]).
-export([set_window_display_mode/2]).
-export([set_window_fullscreen/2]).
-export([set_window_gamma_ramp/4]).
-export([set_window_grab/2]).
-export([set_window_hit_test/3]).
-export([set_window_hit_test_remove/1]).
-export([set_window_hit_test_result/2]).
-export([set_window_icon/2]).
-export([set_window_input_focus/1]).
-export([set_window_maximum_size/3]).
-export([set_window_minimum_size/3]).
-export([set_window_modal_for/2]).
-export([set_window_opacity/2]).
-export([set_window_position/3]).
-export([set_window_resizable/2]).
-export([set_window_size/3]).
-export([set_window_title/2]).
-export([show_window/1]).

%% internal

%% @todo We probably want to accept an env variable or somthing for the location.
-on_load(on_load/0).
on_load() ->
	PrivDir = case code:priv_dir(?MODULE) of
		{error, _} ->
			AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
			filename:join(AppPath, "priv");
		Path ->
			Path
	end,
	erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

register_callback_process() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl

init(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

init_subsystem(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

quit() ->
	erlang:nif_error({not_loaded, ?MODULE}).

quit_subsystem(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_main_ready() ->
	erlang:nif_error({not_loaded, ?MODULE}).

was_init(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_blendmode

compose_custom_blend_mode(_, _, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_clipboard

get_clipboard_text() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_clipboard_text() ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_clipboard_text(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_cpu_info

get_cpu_cache_line_size() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_cpu_count() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_system_ram() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_3dnow() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_altivec() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_avx() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_avx2() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_mmx() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_neon() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_rdtsc() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_sse() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_sse2() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_sse3() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_sse41() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_sse42() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_cursor

create_cursor(_, _, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

create_color_cursor(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

create_system_cursor(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_cursor() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_default_cursor() ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_cursor(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

show_cursor(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_events

flush_event(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

flush_events(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_event(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_events(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

peep_events(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

poll_event() ->
	erlang:nif_error({not_loaded, ?MODULE}).

pump_events() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_filesystem

get_base_path() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_pref_path(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_gl

gl_create_context(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

gl_swap_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_hints

add_hint_callback(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_keyboard

get_key_from_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_key_from_scancode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_key_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_keyboard_focus() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_keyboard_state() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_mod_state() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_scancode_from_key(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_scancode_from_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_scancode_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_screen_keyboard_support() ->
	erlang:nif_error({not_loaded, ?MODULE}).

is_screen_keyboard_shown(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

is_text_input_active() ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_mod_state(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_text_input_rect(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

start_text_input() ->
	erlang:nif_error({not_loaded, ?MODULE}).

stop_text_input() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_mouse

capture_mouse(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_global_mouse_state() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_mouse_focus() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_mouse_state() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_relative_mouse_mode() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_relative_mouse_state() ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_relative_mouse_mode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

warp_mouse_global(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

warp_mouse_in_window(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_platform

get_platform() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_power

get_power_info() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_rect

enclose_points(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_intersection(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

intersect_rect(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

intersect_rect_and_line(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

union_rect(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_renderer

create_renderer(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_num_render_drivers() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_render_draw_blend_mode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_render_draw_color(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_render_driver_info(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_render_output_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_renderer(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_renderer_info(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_clear(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_copy(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_copy_ex(_, _, _, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_draw_line(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_draw_lines(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_draw_point(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_draw_points(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_draw_rect(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_draw_rects(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_fill_rect(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_fill_rects(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_get_clip_rect(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_get_integer_scale(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_get_logical_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_get_scale(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_get_viewport(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_present(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_set_clip_rect(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_set_integer_scale(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_set_logical_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_set_scale(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_set_viewport(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_target_supported(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_render_draw_blend_mode(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_render_draw_color(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_stdinc

get_num_allocations() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_surface

get_surface_dimensions(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

img_load(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_texture

create_texture_from_surface(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_texture_alpha_mod(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_texture_blend_mode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_texture_color_mod(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_texture_alpha_mod(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_texture_blend_mode(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_texture_color_mod(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_ttf

ttf_font_ascent(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_descent(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_face_family_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_face_is_fixed_width(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_face_style_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_faces(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_height(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_font_line_skip(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_get_font_hinting(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_get_font_kerning(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_get_font_kerning_size_glyphs(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_get_font_outline(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_get_font_style(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_glyph_is_provided(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_glyph_metrics(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_init() ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_open_font(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_open_font_index(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_quit() ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_render_utf8_blended(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_render_utf8_blended_wrapped(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_render_utf8_shaded(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_render_utf8_solid(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_set_font_hinting(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_set_font_kerning(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_set_font_outline(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_set_font_style(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_size_utf8(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

ttf_was_init() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_version

get_version() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_revision() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_video

disable_screensaver() ->
	erlang:nif_error({not_loaded, ?MODULE}).

enable_screensaver() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_closest_display_mode(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_current_display_mode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_current_video_driver() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_desktop_display_mode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_display_bounds(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_display_dpi(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_display_mode(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_display_name(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_display_usable_bounds(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_num_display_modes(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_num_video_displays() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_num_video_drivers() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_video_driver(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

is_screensaver_enabled() ->
	erlang:nif_error({not_loaded, ?MODULE}).

video_init(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

video_quit() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_window

create_window(_, _, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

create_window_and_renderer(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_grabbed_window() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_borders_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_brightness(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_display_index(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_display_mode(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_flags(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_from_id(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_gamma_ramp(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_grab(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_id(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_maximum_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_minimum_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_opacity(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_pixel_format(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_position(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_title(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

hide_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

maximize_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

minimize_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

raise_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

restore_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_bordered(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_brightness(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_display_mode(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_fullscreen(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_gamma_ramp(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_grab(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_hit_test(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_hit_test_remove(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_hit_test_result(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_icon(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_input_focus(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_maximum_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_minimum_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_modal_for(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_opacity(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_position(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_resizable(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_title(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

show_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

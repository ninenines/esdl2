%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
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

%% sdl
-export([init/1]).
-export([init_subsystem/1]).
-export([quit/0]).
-export([quit_subsystem/1]).
-export([set_main_ready/0]).
-export([was_init/1]).

%% sdl_clipboard
-export([get_clipboard_text/0]).
-export([has_clipboard_text/0]).
-export([set_clipboard_text/1]).

%% sdl_cpu_info
-export([get_cpu_cache_line_size/0]).
-export([get_cpu_count/0]).
-export([get_system_ram/0]).
-export([has_3dnow/0]).
-export([has_avx/0]).
-export([has_altivec/0]).
-export([has_mmx/0]).
-export([has_rdtsc/0]).
-export([has_sse/0]).
-export([has_sse2/0]).
-export([has_sse3/0]).
-export([has_sse41/0]).
-export([has_sse42/0]).

%% sdl_events
-export([poll_event/0]).

%% sdl_filesystem
-export([get_base_path/0]).
-export([get_pref_path/2]).

%% sdl_gl
-export([gl_create_context/1]).
-export([gl_swap_window/1]).

%% sdl_power
-export([get_power_info/0]).

%% sdl_renderer
-export([create_renderer/3]).
-export([get_num_render_drivers/0]).
-export([get_render_draw_blend_mode/1]).
-export([get_render_draw_color/1]).
-export([get_render_output_size/1]).
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
-export([render_get_logical_size/1]).
-export([render_get_scale/1]).
-export([render_get_viewport/1]).
-export([render_present/1]).
-export([render_set_clip_rect/5]).
-export([render_set_logical_size/3]).
-export([render_set_scale/3]).
-export([render_set_viewport/5]).
-export([render_target_supported/1]).
-export([set_render_draw_blend_mode/2]).
-export([set_render_draw_color/5]).

%% sdl_surface
-export([img_load/1]).

%% sdl_texture
-export([create_texture_from_surface/2]).
-export([get_texture_alpha_mod/1]).
-export([get_texture_blend_mode/1]).
-export([get_texture_color_mod/1]).
-export([set_texture_alpha_mod/2]).
-export([set_texture_blend_mode/2]).
-export([set_texture_color_mod/4]).

%% sdl_version
-export([get_version/0]).
-export([get_revision/0]).

%% sdl_window
-export([create_window/6]).
-export([create_window_and_renderer/3]).
-export([get_window_brightness/1]).
-export([get_window_display_index/1]).
-export([get_window_flags/1]).
-export([get_window_grab/1]).
-export([get_window_id/1]).
-export([get_window_maximum_size/1]).
-export([get_window_minimum_size/1]).
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
-export([set_window_fullscreen/2]).
-export([set_window_grab/2]).
-export([set_window_icon/2]).
-export([set_window_maximum_size/3]).
-export([set_window_minimum_size/3]).
-export([set_window_position/3]).
-export([set_window_size/3]).
-export([set_window_title/2]).
-export([show_window/1]).

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

has_avx() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_altivec() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_mmx() ->
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

%% sdl_events

poll_event() ->
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

%% sdl_power

get_power_info() ->
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

get_render_output_size(_) ->
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

%% sdl_surface

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

%% sdl_version

get_version() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_revision() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_window

create_window(_, _, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

create_window_and_renderer(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_brightness(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_display_index(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_flags(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_grab(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_id(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_maximum_size(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_window_minimum_size(_) ->
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

set_window_fullscreen(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_grab(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_icon(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_maximum_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_minimum_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_position(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_window_title(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

show_window(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

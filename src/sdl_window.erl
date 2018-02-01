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

-module(sdl_window).

-export([create/6]).
-export([create_window_and_renderer/3]).
-export([focus_input/1]).
-export([get_borders_size/1]).
-export([get_brightness/1]).
-export([get_display_index/1]).
-export([get_display_mode/1]).
-export([get_flags/1]).
-export([get_from_id/1]).
-export([get_gamma_ramp/1]).
-export([get_grabbed_window/0]).
-export([get_id/1]).
-export([get_max_size/1]).
-export([get_min_size/1]).
-export([get_opacity/1]).
-export([get_pixel_format/1]).
-export([get_pos/1]).
-export([get_size/1]).
-export([get_title/1]).
-export([grab_input/2]).
-export([hide/1]).
-export([is_input_grabbed/1]).
-export([maximize/1]).
-export([minimize/1]).
-export([raise/1]).
-export([restore/1]).
-export([set_bordered/2]).
-export([set_brightness/2]).
-export([set_display_mode/2]).
-export([set_fullscreen/2]).
-export([set_gamma_ramp/4]).
-export([set_hit_test_callback/3]).
-export([set_icon/2]).
-export([set_max_size/3]).
-export([set_min_size/3]).
-export([set_modal/2]).
-export([set_opacity/2]).
-export([set_pos/3]).
-export([set_resizable/2]).
-export([set_size/3]).
-export([set_title/2]).
-export([show/1]).
-export([unset_hit_test_callback/1]).

-opaque window() :: <<>>.
-export_type([window/0]).

-type window_pos() :: centered | undefined | integer().
-type window_flag() :: fullscreen | fullscreen_desktop | opengl | shown
	| hidden | borderless | resizable | minimized | maximized
	| input_grabbed | input_focus | mouse_focus | foreign | allow_high_dpi
	| mouse_capture | always_on_top | skip_taskbar | utility
	| tooltip | popup_menu | vulkan.

-type window_event_type() :: shown | hidden | exposed | moved | resized
	| size_changed | minimized | maximized | restored | enter | leave
	| focus_gained | focus_lost | close | take_focus | hit_test.
-export_type([window_event_type/0]).

%% It must be a list of 256 elements.
-type gamma_ramp() :: [0..65535].

-spec create(binary(), window_pos(), window_pos(), integer(), integer(), [window_flag()])
	-> {ok, window()} | sdl:error().
create(Title, X, Y, W, H, Flags) ->
	esdl2:create_window(Title, X, Y, W, H, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec create_window_and_renderer(integer(), integer(), [window_flag()])
	-> {ok, window(), sdl_renderer:renderer()} | sdl:error().
create_window_and_renderer(W, H, Flags) ->
	esdl2:create_window_and_renderer(W, H, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec focus_input(window()) -> ok | sdl:error().
focus_input(Window) ->
	esdl2:set_window_input_focus(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_borders_size(window())
	-> #{top := integer(), left := integer(), bottom := integer(), right := integer()}
	| undefined.
get_borders_size(Window) ->
	esdl2:get_window_borders_size(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_brightness(window()) -> float().
get_brightness(Window) ->
	esdl2:get_window_brightness(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_display_mode(window()) -> sdl_video:display_mode().
get_display_mode(Window) ->
	esdl2:get_window_display_mode(Window),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

-spec get_display_index(window()) -> integer().
get_display_index(Window) ->
	esdl2:get_window_display_index(Window),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Index} = Ret,
		Index
	end.

-spec get_flags(window()) -> [window_flag()].
get_flags(Window) ->
	esdl2:get_window_flags(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_from_id(non_neg_integer()) -> window() | undefined.
get_from_id(WindowID) ->
	esdl2:get_window_from_id(WindowID),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_gamma_ramp(window()) -> {gamma_ramp(), gamma_ramp(), gamma_ramp()}.
get_gamma_ramp(Window) ->
	esdl2:get_window_gamma_ramp(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_grabbed_window() -> window() | undefined.
get_grabbed_window() ->
	esdl2:get_grabbed_window(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_id(window()) -> non_neg_integer().
get_id(Window) ->
	esdl2:get_window_id(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_max_size(window()) -> {integer(), integer()}.
get_max_size(Window) ->
	esdl2:get_window_maximum_size(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_min_size(window()) -> {integer(), integer()}.
get_min_size(Window) ->
	esdl2:get_window_minimum_size(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_opacity(window()) -> float().
get_opacity(Window) ->
	esdl2:get_window_opacity(Window),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Opacity} = Ret,
		Opacity
	end.

-spec get_pixel_format(window()) -> sdl_pixels:pixel_format().
get_pixel_format(Window) ->
	esdl2:get_window_pixel_format(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_pos(window()) -> {integer(), integer()}.
get_pos(Window) ->
	esdl2:get_window_position(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_size(window()) -> {integer(), integer()}.
get_size(Window) ->
	esdl2:get_window_size(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_title(window()) -> binary().
get_title(Window) ->
	esdl2:get_window_title(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec grab_input(window(), boolean()) -> ok.
grab_input(Window, Grab) ->
	esdl2:set_window_grab(Window, Grab).

-spec hide(window()) -> ok.
hide(Window) ->
	esdl2:hide_window(Window).

-spec is_input_grabbed(window()) -> boolean().
is_input_grabbed(Window) ->
	esdl2:get_window_grab(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec maximize(window()) -> ok.
maximize(Window) ->
	esdl2:maximize_window(Window).

-spec minimize(window()) -> ok.
minimize(Window) ->
	esdl2:minimize_window(Window).

-spec raise(window()) -> ok.
raise(Window) ->
	esdl2:raise_window(Window).

-spec restore(window()) -> ok.
restore(Window) ->
	esdl2:restore_window(Window).

-spec set_bordered(window(), boolean()) -> ok.
set_bordered(Window, Bordered) ->
	esdl2:set_window_bordered(Window, Bordered).

-spec set_brightness(window(), float()) -> ok | sdl:error().
set_brightness(Window, Brightness) ->
	esdl2:set_window_brightness(Window, Brightness),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_display_mode(window(), sdl_video:display_mode()) -> ok | sdl:error().
set_display_mode(Window, Mode) ->
	esdl2:set_window_display_mode(Window, Mode),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_fullscreen(window(), fullscreen | fullscreen_desktop | windowed)
	-> ok | sdl:error().
set_fullscreen(Window, Flag) ->
	esdl2:set_window_fullscreen(Window, Flag),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_gamma_ramp(window(), gamma_ramp(), gamma_ramp(), gamma_ramp())
	-> ok | sdl:error().
set_gamma_ramp(Window, Red, Green, Blue) ->
	esdl2:set_window_gamma_ramp(Window, Red, Green, Blue),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_hit_test_callback(window(), module(), atom()) -> ok | sdl:error().
set_hit_test_callback(Window, Module, Function) ->
	esdl2:set_window_hit_test(Window, Module, Function),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_icon(window(), sdl_surface:surface()) -> ok.
set_icon(Window, Surface) ->
	esdl2:set_window_icon(Window, Surface),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_max_size(window(), integer(), integer()) -> ok.
set_max_size(Window, W, H) ->
	esdl2:set_window_maximum_size(Window, W, H).

-spec set_min_size(window(), integer(), integer()) -> ok.
set_min_size(Window, W, H) ->
	esdl2:set_window_minimum_size(Window, W, H).

-spec set_modal(window(), window()) -> ok | sdl:error().
set_modal(ModalWindow, ParentWindow) ->
	esdl2:set_window_modal_for(ModalWindow, ParentWindow),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_opacity(window(), float()) -> ok | sdl:error().
set_opacity(Window, Opacity) ->
	esdl2:set_window_opacity(Window, Opacity),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_pos(window(), integer(), integer()) -> ok.
set_pos(Window, X, Y) ->
	esdl2:set_window_position(Window, X, Y).

-spec set_resizable(window(), boolean()) -> ok.
set_resizable(Window, Resizable) ->
	esdl2:set_window_resizable(Window, Resizable).

-spec set_size(window(), integer(), integer()) -> ok.
set_size(Window, W, H) ->
	esdl2:set_window_size(Window, W, H).

-spec set_title(window(), binary()) -> ok.
set_title(Window, Title) ->
	esdl2:set_window_title(Window, Title).

-spec show(window()) -> ok.
show(Window) ->
	esdl2:show_window(Window).

-spec unset_hit_test_callback(window()) -> ok | sdl:error().
unset_hit_test_callback(Window) ->
	esdl2:set_window_hit_test_remove(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

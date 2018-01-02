%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(sdl_mouse).

-export([capture/1]).
-export([get_focused_window/0]).
-export([get_global_state/0]).
-export([get_relative_mode/0]).
-export([get_relative_state/0]).
-export([get_state/0]).
-export([set_relative_mode/1]).
-export([warp/2]).
-export([warp/3]).

-type wheel_direction() :: normal | flipped.
-export_type([wheel_direction/0]).

-type button() :: left | middle | right | x1 | x2.
-export_type([button/0]).

-spec capture(boolean()) -> ok | sdl:error().
capture(Bool) ->
	esdl2:capture_mouse(Bool),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_focused_window() -> sdl_window:window() | undefined.
get_focused_window() ->
	esdl2:get_mouse_focus(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_global_state() -> {integer(), integer(), [button()]}.
get_global_state() ->
	esdl2:get_global_mouse_state(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_relative_mode() -> boolean().
get_relative_mode() ->
	esdl2:get_relative_mouse_mode(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_relative_state() -> {integer(), integer(), [button()]}.
get_relative_state() ->
	esdl2:get_relative_mouse_state(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_state() -> {integer(), integer(), [button()]}.
get_state() ->
	esdl2:get_mouse_state(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_relative_mode(boolean()) -> ok | sdl:error().
set_relative_mode(Bool) ->
	esdl2:set_relative_mouse_mode(Bool),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec warp(integer(), integer()) -> ok | sdl:error().
warp(X, Y) ->
	esdl2:warp_mouse_global(X, Y),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec warp(sdl_window:window(), integer(), integer()) -> ok.
warp(Window, X, Y) ->
	esdl2:warp_mouse_in_window(Window, X, Y).

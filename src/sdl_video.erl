%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(sdl_video).

-export([get_closest_display_mode/2]).
-export([get_current_display_mode/1]).
-export([get_current_driver/0]).
-export([get_desktop_display_mode/1]).
-export([get_display_bounds/1]).
-export([get_display_dpi/1]).
-export([get_display_mode/2]).
-export([get_display_name/1]).
-export([get_display_usable_bounds/1]).
-export([get_driver/1]).
-export([get_num_display_modes/1]).
-export([get_num_displays/0]).
-export([get_num_drivers/0]).
-export([start/1]).
-export([stop/0]).

-type display_mode() :: #{
	format => sdl_pixels:pixel_format(),
	w => integer(),
	h => integer(),
	refresh_rate => integer()
}.

-spec get_closest_display_mode(integer(), display_mode()) -> display_mode() | undefined.
get_closest_display_mode(DisplayIndex, Mode) ->
	esdl2:get_closest_display_mode(DisplayIndex, Mode),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_current_display_mode(integer()) -> display_mode() | undefined.
get_current_display_mode(DisplayIndex) ->
	esdl2:get_current_display_mode(DisplayIndex),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_current_driver() -> binary() | undefined.
get_current_driver() ->
	esdl2:get_current_video_driver(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_desktop_display_mode(integer()) -> display_mode() | undefined.
get_desktop_display_mode(DisplayIndex) ->
	esdl2:get_desktop_display_mode(DisplayIndex),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_display_bounds(integer()) -> sdl_rect:rect() | undefined.
get_display_bounds(Index) ->
	esdl2:get_display_bounds(Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_display_dpi(integer())
	-> #{diagonal := float(), horizontal := float(), vertical := float()}
	| undefined.
get_display_dpi(Index) ->
	esdl2:get_display_dpi(Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_display_mode(integer(), integer()) -> display_mode() | undefined.
get_display_mode(DisplayIndex, Index) ->
	esdl2:get_display_mode(DisplayIndex, Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_display_name(integer()) -> binary() | undefined.
get_display_name(Index) ->
	esdl2:get_display_name(Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_display_usable_bounds(integer()) -> sdl_rect:rect() | undefined.
get_display_usable_bounds(Index) ->
	esdl2:get_display_usable_bounds(Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_driver(integer()) -> binary() | undefined.
get_driver(Index) ->
	esdl2:get_video_driver(Index).

-spec get_num_display_modes(integer()) -> integer().
get_num_display_modes(DisplayIndex) ->
	esdl2:get_num_display_modes(DisplayIndex),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_num_displays() -> integer().
get_num_displays() ->
	esdl2:get_num_video_displays(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_num_drivers() -> integer().
get_num_drivers() ->
	esdl2:get_num_video_drivers().

-spec start(binary()) -> ok | sdl:error().
start(DriverName) ->
	esdl2:video_init(DriverName),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec stop() -> ok.
stop() ->
	esdl2:video_quit().

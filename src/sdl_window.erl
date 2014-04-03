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

-module(sdl_window).

-export([create/6]).
-export([create_window_and_renderer/3]).
-export([get_brightness/1]).
-export([get_display_index/1]).
-export([get_flags/1]).
-export([is_input_grabbed/1]).
-export([get_id/1]).
-export([get_max_size/1]).

create(Title, X, Y, W, H, Flags) ->
	esdl2:create_window(Title, X, Y, W, H, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

create_window_and_renderer(W, H, Flags) ->
	esdl2:create_window_and_renderer(W, H, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_brightness(Window) ->
	esdl2:get_window_brightness(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_display_index(Window) ->
	esdl2:get_window_display_index(Window),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Index} = Ret,
		Index
	end.

get_flags(Window) ->
	esdl2:get_window_flags(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

is_input_grabbed(Window) ->
	esdl2:get_window_grab(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_id(Window) ->
	esdl2:get_window_id(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_max_size(Window) ->
	esdl2:get_window_maximum_size(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

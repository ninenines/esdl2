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

-module(sdl_ttf).

-export([is_started/0]).
-export([open_font/2]).
-export([render_solid/3]).
-export([start/0]).
-export([stop/0]).

-opaque font() :: reference().
-export_type([font/0]).

-spec is_started() -> boolean().
is_started() ->
	esdl2:ttf_was_init(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec open_font(binary(), pos_integer()) -> {ok, font()} | sdl:error().
open_font(Filename, PointSize) ->
	esdl2:ttf_open_font(Filename, PointSize),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec render_solid(font(), binary(), sdl_pixels:color())
	-> {ok, sdl_surface:surface()} | sdl:error().
render_solid(Font, Text, Color) ->
	esdl2:ttf_render_utf8_solid(Font, Text, Color),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec start() -> ok | sdl:error().
start() ->
	esdl2:ttf_init(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec stop() -> ok.
stop() ->
	esdl2:ttf_quit().

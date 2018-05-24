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

-module(sdl_cursor).

-export([create/1]).
-export([create/3]).
-export([create/6]).
-export([get_current/0]).
-export([get_default/0]).
-export([hide/0]).
-export([is_visible/0]).
-export([set/1]).
-export([show/0]).

-opaque cursor() :: <<>>.
-export_type([cursor/0]).

-type system_cursor() :: arrow | ibeam | wait | crosshair | wait_arrow
	| size_nwse | size_nesw | size_we | size_ns | size_all | no | hand.

-spec create(system_cursor()) -> {ok, cursor()} | sdl:error().
create(ID) ->
	esdl2:create_system_cursor(ID),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec create(sdl_surface:surface(), integer(), integer())
	-> {ok, cursor()} | sdl:error().
create(Surface, HotX, HotY) ->
	esdl2:create_color_cursor(Surface, HotX, HotY),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec create(binary(), binary(), integer(), integer(), integer(), integer())
	-> {ok, cursor()} | sdl:error().
create(Data, Mask, W, H, HotX, HotY) ->
	esdl2:create_cursor(Data, Mask, W, H, HotX, HotY),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_current() -> cursor().
get_current() ->
	esdl2:get_cursor(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_default() -> cursor().
get_default() ->
	esdl2:get_default_cursor(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec hide() -> ok.
hide() ->
	esdl2:show_cursor(0),
	receive {'_nif_thread_ret_', _} -> ok end.

-spec is_visible() -> boolean().
is_visible() ->
	esdl2:show_cursor(-1),
	receive
		{'_nif_thread_ret_', 0} -> false;
		{'_nif_thread_ret_', 1} -> true
	end.

-spec set(cursor()) -> ok.
set(Cursor) ->
	esdl2:set_cursor(Cursor).

-spec show() -> ok.
show() ->
	esdl2:show_cursor(1),
	receive {'_nif_thread_ret_', _} -> ok end.

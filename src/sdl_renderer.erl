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

-module(sdl_renderer).

-export([create/3]).
-export([clear/1]).
-export([copy/2]).
-export([copy/4]).
-export([present/1]).
-export([set_draw_color/5]).
-export([set_logical_size/3]).

create(Window, Index, Flags) ->
	esdl2:create_renderer(Window, Index, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

clear(Renderer) ->
	esdl2:render_clear(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

copy(Renderer, Texture) ->
	esdl2:render_copy(Renderer, Texture, undefined, undefined),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

copy(Renderer, Texture, SrcRect, DstRect) ->
	esdl2:render_copy(Renderer, Texture, SrcRect, DstRect),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

present(Renderer) ->
	esdl2:render_present(Renderer).

set_draw_color(Renderer, R, G, B, A) ->
	esdl2:set_render_draw_color(Renderer, R, G, B, A),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

set_logical_size(Renderer, W, H) ->
	esdl2:render_set_logical_size(Renderer, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

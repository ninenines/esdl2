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
-export([count_drivers/0]).
-export([get_draw_blend_mode/1]).
-export([get_draw_color/1]).
-export([get_output_size/1]).
-export([clear/1]).
-export([copy/2]).
-export([copy/4]).
-export([copy/7]).
-export([draw_line/3]).
-export([draw_line/5]).
-export([draw_lines/2]).
-export([draw_point/2]).
-export([draw_point/3]).
-export([draw_points/2]).
-export([draw_rect/2]).
-export([draw_rect/5]).
-export([draw_rects/2]).
-export([fill_rect/2]).
-export([fill_rect/5]).
-export([fill_rects/2]).
-export([get_clip_rect/1]).
-export([get_logical_size/1]).
-export([get_scale/1]).
-export([present/1]).
-export([set_draw_color/5]).
-export([set_logical_size/3]).

create(Window, Index, Flags) ->
	esdl2:create_renderer(Window, Index, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

count_drivers() ->
	{ok, Count} = esdl2:get_num_render_drivers(),
	Count.

get_draw_blend_mode(Renderer) ->
	esdl2:get_render_draw_blend_mode(Renderer),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

get_draw_color(Renderer) ->
	esdl2:get_render_draw_color(Renderer),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

get_output_size(Renderer) ->
	esdl2:get_render_output_size(Renderer),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

clear(Renderer) ->
	esdl2:render_clear(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

copy(Renderer, Texture) ->
	esdl2:render_copy(Renderer, Texture, undefined, undefined),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

copy(Renderer, Texture, SrcRect, DstRect) ->
	esdl2:render_copy(Renderer, Texture, SrcRect, DstRect),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

copy(Renderer, Texture, SrcRect, DstRect, Angle, CenterPoint, FlipFlags) ->
	esdl2:render_copy_ex(Renderer, Texture, SrcRect, DstRect, Angle, CenterPoint, FlipFlags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

draw_line(Renderer, #{x:=X1, y:=Y1}, #{x:=X2, y:=Y2}) ->
	draw_line(Renderer, X1, Y1, X2, Y2).

draw_line(Renderer, X1, Y1, X2, Y2) ->
	esdl2:render_draw_line(Renderer, X1, Y1, X2, Y2),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

draw_lines(Renderer, Points) ->
	esdl2:render_draw_lines(Renderer, Points),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

draw_point(Renderer, #{x:=X, y:=Y}) ->
	draw_point(Renderer, X, Y).

draw_point(Renderer, X, Y) ->
	esdl2:render_draw_point(Renderer, X, Y),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

draw_points(Renderer, Points) ->
	esdl2:render_draw_points(Renderer, Points),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

draw_rect(Renderer, #{x:=X, y:=Y, w:=W, h:=H}) ->
	draw_rect(Renderer, X, Y, W, H).

draw_rect(Renderer, X, Y, W, H) ->
	esdl2:render_draw_rect(Renderer, X, Y, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

draw_rects(Renderer, Rects) ->
	esdl2:render_draw_rects(Renderer, Rects),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

fill_rect(Renderer, #{x:=X, y:=Y, w:=W, h:=H}) ->
	fill_rect(Renderer, X, Y, W, H).

fill_rect(Renderer, X, Y, W, H) ->
	esdl2:render_fill_rect(Renderer, X, Y, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

fill_rects(Renderer, Rects) ->
	esdl2:render_fill_rects(Renderer, Rects),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_clip_rect(Renderer) ->
	esdl2:render_get_clip_rect(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_logical_size(Renderer) ->
	esdl2:render_get_logical_size(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_scale(Renderer) ->
	esdl2:render_get_scale(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

present(Renderer) ->
	esdl2:render_present(Renderer).

set_draw_color(Renderer, R, G, B, A) ->
	esdl2:set_render_draw_color(Renderer, R, G, B, A),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

set_logical_size(Renderer, W, H) ->
	esdl2:render_set_logical_size(Renderer, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

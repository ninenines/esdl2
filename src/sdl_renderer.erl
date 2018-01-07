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

-module(sdl_renderer).

-export([clear/1]).
-export([copy/2]).
-export([copy/4]).
-export([copy/7]).
-export([create/3]).
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
-export([get_draw_blend_mode/1]).
-export([get_draw_color/1]).
-export([get_driver_info/1]).
-export([get_from_window/1]).
-export([get_info/1]).
-export([get_integer_scale/1]).
-export([get_logical_size/1]).
-export([get_num_drivers/0]).
-export([get_output_size/1]).
-export([get_scale/1]).
-export([get_viewport/1]).
-export([is_target_supported/1]).
-export([present/1]).
-export([set_clip_rect/2]).
-export([set_clip_rect/5]).
-export([set_draw_blend_mode/2]).
-export([set_draw_color/5]).
-export([set_integer_scale/2]).
-export([set_logical_size/3]).
-export([set_scale/3]).
-export([set_viewport/2]).
-export([set_viewport/5]).

-opaque renderer() :: <<>>.
-export_type([renderer/0]).

-type renderer_flag() :: software | accelerated | present_vsync | target_texture.
-export_type([renderer_flag/0]).

-type renderer_info() :: #{
	name => binary(),
	flags => [renderer_flag()],
	texture_formats => sdl_pixels:pixel_format(),
	max_texture_width => integer(),
	max_texture_height => integer()
}.

-spec clear(renderer()) -> ok | sdl:error().
clear(Renderer) ->
	esdl2:render_clear(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec copy(renderer(), sdl_texture:texture()) -> ok | sdl:error().
copy(Renderer, Texture) ->
	esdl2:render_copy(Renderer, Texture, undefined, undefined),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec copy(renderer(), sdl_texture:texture(),
	undefined | sdl_rect:rect(), undefined | sdl_rect:rect())
	-> ok | sdl:error().
copy(Renderer, Texture, SrcRect, DstRect) ->
	esdl2:render_copy(Renderer, Texture, SrcRect, DstRect),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec copy(renderer(), sdl_texture:texture(),
	undefined | sdl_rect:rect(), undefined | sdl_rect:rect(),
	float(), undefined | sdl_rect:point(), none | horizontal | vertical)
	-> ok | sdl:error().
copy(Renderer, Texture, SrcRect, DstRect, Angle, CenterPoint, FlipFlags) ->
	esdl2:render_copy_ex(Renderer, Texture, SrcRect, DstRect, Angle, CenterPoint, FlipFlags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec create(sdl_window:window(), integer(), [renderer_flag()])
	-> {ok, renderer()} | sdl:error().
create(Window, Index, Flags) ->
	esdl2:create_renderer(Window, Index, Flags),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec draw_line(renderer(), sdl_rect:point(), sdl_rect:point()) -> ok | sdl:error().
draw_line(Renderer, #{x:=X1, y:=Y1}, #{x:=X2, y:=Y2}) ->
	draw_line(Renderer, X1, Y1, X2, Y2).

-spec draw_line(renderer(), integer(), integer(), integer(), integer()) -> ok | sdl:error().
draw_line(Renderer, X1, Y1, X2, Y2) ->
	esdl2:render_draw_line(Renderer, X1, Y1, X2, Y2),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec draw_lines(renderer(), [sdl_rect:point()]) -> ok | sdl:error().
draw_lines(Renderer, Points) ->
	esdl2:render_draw_lines(Renderer, Points),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec draw_point(renderer(), sdl_rect:point()) -> ok | sdl:error().
draw_point(Renderer, #{x:=X, y:=Y}) ->
	draw_point(Renderer, X, Y).

-spec draw_point(renderer(), integer(), integer()) -> ok | sdl:error().
draw_point(Renderer, X, Y) ->
	esdl2:render_draw_point(Renderer, X, Y),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec draw_points(renderer(), [sdl_rect:point()]) -> ok | sdl:error().
draw_points(Renderer, Points) ->
	esdl2:render_draw_points(Renderer, Points),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec draw_rect(renderer(), sdl_rect:rect()) -> ok | sdl:error().
draw_rect(Renderer, #{x:=X, y:=Y, w:=W, h:=H}) ->
	draw_rect(Renderer, X, Y, W, H).

-spec draw_rect(renderer(), integer(), integer(), integer(), integer()) -> ok | sdl:error().
draw_rect(Renderer, X, Y, W, H) ->
	esdl2:render_draw_rect(Renderer, X, Y, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec draw_rects(renderer(), [sdl_rect:rect()]) -> ok | sdl:error().
draw_rects(Renderer, Rects) ->
	esdl2:render_draw_rects(Renderer, Rects),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec fill_rect(renderer(), sdl_rect:rect()) -> ok | sdl:error().
fill_rect(Renderer, #{x:=X, y:=Y, w:=W, h:=H}) ->
	fill_rect(Renderer, X, Y, W, H).

-spec fill_rect(renderer(), integer(), integer(), integer(), integer()) -> ok | sdl:error().
fill_rect(Renderer, X, Y, W, H) ->
	esdl2:render_fill_rect(Renderer, X, Y, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec fill_rects(renderer(), [sdl_rect:rect()]) -> ok | sdl:error().
fill_rects(Renderer, Rects) ->
	esdl2:render_fill_rects(Renderer, Rects),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_clip_rect(renderer()) -> sdl_rect:rect().
get_clip_rect(Renderer) ->
	esdl2:render_get_clip_rect(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_draw_blend_mode(renderer()) -> sdl_blend_mode:blend_mode().
get_draw_blend_mode(Renderer) ->
	esdl2:get_render_draw_blend_mode(Renderer),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

-spec get_draw_color(renderer()) -> {byte(), byte(), byte(), byte()}.
get_draw_color(Renderer) ->
	esdl2:get_render_draw_color(Renderer),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

-spec get_driver_info(integer()) -> {ok, renderer_info()} | sdl:error().
get_driver_info(Index) ->
	esdl2:get_render_driver_info(Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_from_window(sdl_window:window()) -> renderer().
get_from_window(Window) ->
	esdl2:get_renderer(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_info(renderer()) -> {ok, renderer_info()} | sdl:error().
get_info(Renderer) ->
	esdl2:get_renderer_info(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_integer_scale(renderer()) -> boolean().
get_integer_scale(Renderer) ->
	esdl2:render_get_integer_scale(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_logical_size(renderer()) -> {integer(), integer()}.
get_logical_size(Renderer) ->
	esdl2:render_get_logical_size(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_num_drivers() -> integer().
get_num_drivers() ->
	{ok, Num} = esdl2:get_num_render_drivers(),
	Num.

-spec get_output_size(renderer()) -> {integer(), integer()}.
get_output_size(Renderer) ->
	esdl2:get_render_output_size(Renderer),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Mode} = Ret,
		Mode
	end.

-spec get_scale(renderer()) -> {float(), float()}.
get_scale(Renderer) ->
	esdl2:render_get_scale(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_viewport(renderer()) -> sdl_rect:rect().
get_viewport(Renderer) ->
	esdl2:render_get_viewport(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec is_target_supported(renderer()) -> boolean().
is_target_supported(Renderer) ->
	esdl2:render_target_supported(Renderer),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec present(renderer()) -> ok.
present(Renderer) ->
	esdl2:render_present(Renderer).

-spec set_clip_rect(renderer(), sdl_rect:rect()) -> ok | sdl:error().
set_clip_rect(Renderer, #{x:=X, y:=Y, w:=W, h:=H}) ->
	set_clip_rect(Renderer, X, Y, W, H).

-spec set_clip_rect(renderer(), integer(), integer(), integer(), integer()) -> ok | sdl:error().
set_clip_rect(Renderer, X, Y, W, H) ->
	esdl2:render_set_clip_rect(Renderer, X, Y, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_draw_blend_mode(renderer(), sdl_blend_mode:blend_mode()) -> ok | sdl:error().
set_draw_blend_mode(Renderer, BlendMode) ->
	esdl2:set_render_draw_blend_mode(Renderer, BlendMode),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_draw_color(renderer(), byte(), byte(), byte(), byte()) -> ok | sdl:error().
set_draw_color(Renderer, R, G, B, A) ->
	esdl2:set_render_draw_color(Renderer, R, G, B, A),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_integer_scale(renderer(), boolean()) -> ok | sdl:error().
set_integer_scale(Renderer, Bool) ->
	esdl2:render_set_integer_scale(Renderer, Bool),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_logical_size(renderer(), integer(), integer()) -> ok | sdl:error().
set_logical_size(Renderer, W, H) ->
	esdl2:render_set_logical_size(Renderer, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_scale(renderer(), float(), float()) -> ok | sdl:error().
set_scale(Renderer, ScaleX, ScaleY) ->
	esdl2:render_set_scale(Renderer, ScaleX, ScaleY),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_viewport(renderer(), sdl_rect:rect()) -> ok | sdl:error().
set_viewport(Renderer, #{x:=X, y:=Y, w:=W, h:=H}) ->
	set_viewport(Renderer, X, Y, W, H).

-spec set_viewport(renderer(), integer(), integer(), integer(), integer()) -> ok | sdl:error().
set_viewport(Renderer, X, Y, W, H) ->
	esdl2:render_set_viewport(Renderer, X, Y, W, H),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

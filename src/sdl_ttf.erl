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

-export([disable_kerning/1]).
-export([enable_kerning/1]).
-export([get_ascent/1]).
-export([get_descent/1]).
-export([get_family_name/1]).
-export([get_glyph_metrics/2]).
-export([get_glyphs_kerning_size/3]).
-export([get_height/1]).
-export([get_hinting/1]).
-export([get_line_skip/1]).
-export([get_num_faces/1]).
-export([get_outline/1]).
-export([get_style/1]).
-export([get_style_name/1]).
-export([has_glyph/2]).
-export([is_fixed_width/1]).
-export([is_kerning_enabled/1]).
-export([is_started/0]).
-export([open_font/2]).
-export([open_font/3]).
-export([render_blended/3]).
-export([render_blended/4]).
-export([render_shaded/4]).
-export([render_solid/3]).
-export([render_size/2]).
-export([set_hinting/2]).
-export([set_outline/2]).
-export([set_style/2]).
-export([start/0]).
-export([stop/0]).

-opaque font() :: reference().
-export_type([font/0]).

-type glyph() :: 0..65535.
-export_type([glyph/0]).

-type hinting() :: normal | light | mono | none.
-export_type([hinting/0]).

-type style() :: [normal | bold | italic | underline | strikethrough].
-export_type([style/0]).

-spec disable_kerning(font()) -> ok.
disable_kerning(Font) ->
	esdl2:ttf_set_font_kerning(Font, false).

-spec enable_kerning(font()) -> ok.
enable_kerning(Font) ->
	esdl2:ttf_set_font_kerning(Font, true).

-spec get_ascent(font()) -> non_neg_integer().
get_ascent(Font) ->
	esdl2:ttf_font_ascent(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_descent(font()) -> neg_integer().
get_descent(Font) ->
	esdl2:ttf_font_descent(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_family_name(font()) -> binary().
get_family_name(Font) ->
	esdl2:ttf_font_face_family_name(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_glyph_metrics(font(), glyph())
	-> {ok, {integer(), integer(), integer(), integer(), integer()}}
	| sdl:error().
get_glyph_metrics(Font, Glyph) ->
	esdl2:ttf_glyph_metrics(Font, Glyph),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_glyphs_kerning_size(font(), glyph(), glyph())
	-> {ok, integer()} | sdl:error().
get_glyphs_kerning_size(Font, PreviousGlyph, Glyph) ->
	esdl2:ttf_get_font_kerning_size_glyphs(Font, PreviousGlyph, Glyph),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_height(font()) -> non_neg_integer().
get_height(Font) ->
	esdl2:ttf_font_height(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_hinting(font()) -> hinting().
get_hinting(Font) ->
	esdl2:ttf_get_font_hinting(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_line_skip(font()) -> non_neg_integer().
get_line_skip(Font) ->
	esdl2:ttf_font_line_skip(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_num_faces(font()) -> non_neg_integer().
get_num_faces(Font) ->
	esdl2:ttf_font_faces(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_outline(font()) -> non_neg_integer().
get_outline(Font) ->
	esdl2:ttf_get_font_outline(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_style(font()) -> style().
get_style(Font) ->
	esdl2:ttf_get_font_style(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_style_name(font()) -> binary().
get_style_name(Font) ->
	esdl2:ttf_font_face_style_name(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec has_glyph(font(), glyph()) -> boolean().
has_glyph(Font, Glyph) ->
	esdl2:ttf_glyph_is_provided(Font, Glyph),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec is_fixed_width(font()) -> boolean().
is_fixed_width(Font) ->
	esdl2:ttf_font_face_is_fixed_width(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec is_kerning_enabled(font()) -> boolean().
is_kerning_enabled(Font) ->
	esdl2:ttf_get_font_kerning(Font),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec is_started() -> boolean().
is_started() ->
	esdl2:ttf_was_init(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec open_font(binary(), pos_integer()) -> {ok, font()} | sdl:error().
open_font(Filename, PointSize) ->
	esdl2:ttf_open_font(Filename, PointSize),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec open_font(binary(), pos_integer(), non_neg_integer()) -> {ok, font()} | sdl:error().
open_font(Filename, PointSize, Index) ->
	esdl2:ttf_open_font_index(Filename, PointSize, Index),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec render_blended(font(), binary() | glyph(), sdl_pixels:color())
	-> {ok, sdl_surface:surface()} | sdl:error().
render_blended(Font, Text, Fg) when is_binary(Text) ->
	esdl2:ttf_render_utf8_blended(Font, Text, Fg),
	receive {'_nif_thread_ret_', Ret} -> Ret end;
render_blended(Font, Glyph, Fg) ->
	esdl2:ttf_render_glyph_blended(Font, Glyph, Fg),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec render_blended(font(), binary(), sdl_pixels:color(), non_neg_integer())
	-> {ok, sdl_surface:surface()} | sdl:error().
render_blended(Font, Text, Fg, WrapLen) ->
	esdl2:ttf_render_utf8_blended_wrapped(Font, Text, Fg, WrapLen),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec render_shaded(font(), binary() | glyph(), sdl_pixels:color(), sdl_pixels:color())
	-> {ok, sdl_surface:surface()} | sdl:error().
render_shaded(Font, Text, Fg, Bg) when is_binary(Text) ->
	esdl2:ttf_render_utf8_shaded(Font, Text, Fg, Bg),
	receive {'_nif_thread_ret_', Ret} -> Ret end;
render_shaded(Font, Glyph, Fg, Bg) ->
	esdl2:ttf_render_glyph_shaded(Font, Glyph, Fg, Bg),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec render_solid(font(), binary() | glyph(), sdl_pixels:color())
	-> {ok, sdl_surface:surface()} | sdl:error().
render_solid(Font, Text, Fg) when is_binary(Text) ->
	esdl2:ttf_render_utf8_solid(Font, Text, Fg),
	receive {'_nif_thread_ret_', Ret} -> Ret end;
render_solid(Font, Glyph, Fg) ->
	esdl2:ttf_render_glyph_solid(Font, Glyph, Fg),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec render_size(font(), binary())
	-> {ok, non_neg_integer(), non_neg_integer()} | sdl:error().
render_size(Font, Text) ->
	esdl2:ttf_size_utf8(Font, Text),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_hinting(font(), hinting()) -> ok.
set_hinting(Font, Hinting) ->
	esdl2:ttf_set_font_hinting(Font, Hinting).

-spec set_outline(font(), non_neg_integer()) -> ok.
set_outline(Font, Outline) ->
	esdl2:ttf_set_font_outline(Font, Outline).

-spec set_style(font(), style()) -> ok.
set_style(Font, Style) ->
	esdl2:ttf_set_font_style(Font, Style).

-spec start() -> ok | sdl:error().
start() ->
	esdl2:ttf_init(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec stop() -> ok.
stop() ->
	esdl2:ttf_quit().

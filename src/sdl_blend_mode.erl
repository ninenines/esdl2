%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(sdl_blend_mode).

-export([compose/6]).

-type blend_mode() :: none | blend | add | mod | invalid | integer().
-export_type([blend_mode/0]).

-type blend_operation() :: add | substract | rev_substract | minimum | maximum.
-type blend_factor() :: zero | one
	| src_color | one_minus_src_color
	| src_alpha | one_minus_src_alpha
	| dst_color | one_minus_dst_color
	| dst_alpha | one_minus_dst_alpha.

-spec compose(blend_factor(), blend_factor(), blend_operation(),
	blend_factor(), blend_factor(), blend_operation()) -> blend_mode().
compose(SrcColorFactor, DstColorFactor, ColorOp,
		SrcAlphaFactor, DstAlphaFactor, AlphaOp) ->
	esdl2:compose_custom_blend_mode(
		SrcColorFactor, DstColorFactor, ColorOp,
		SrcAlphaFactor, DstAlphaFactor, AlphaOp),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

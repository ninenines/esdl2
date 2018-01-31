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

%% The SDL_FORCE_INLINE functions have been reimplemented in Erlang.
-module(sdl_rect).

-export([enclose_points/2]).
-export([has_intersection/2]).
-export([intersection/2]).
-export([intersection/3]).
-export([intersection/5]).
-export([is_empty/1]).
-export([is_equal/2]).
-export([is_in/2]).
-export([union/2]).

-type point() :: #{x=>integer(), y=>integer()}.
-export_type([point/0]).

-type rect() :: #{x=>integer(), y=>integer(), w=>integer(), h=>integer()}.
-export_type([rect/0]).

-spec enclose_points([point()], rect()) -> rect() | false.
enclose_points(Points, ClipRect) ->
	esdl2:enclose_points(Points, ClipRect).

-spec has_intersection(rect(), rect()) -> boolean().
has_intersection(Rect1, Rect2) ->
	esdl2:has_intersection(Rect1, Rect2).

-spec intersection(rect(), rect()) -> rect() | false.
intersection(Rect1, Rect2) ->
	esdl2:intersect_rect(Rect1, Rect2).

-spec intersection(rect(), P1, P2) -> {P1, P2} | false when P1 :: point(), P2 :: point().
intersection(Rect, #{x := X1, y := Y1}, #{x := X2, y := Y2}) ->
	case intersection(Rect, X1, Y1, X2, Y2) of
		false ->
			false;
		{ResX1, ResY1, ResX2, ResY2} ->
			{#{x => ResX1, y => ResY1}, #{x => ResX2, y => ResY2}}
	end.

-spec intersection(rect(), X1, Y1, X2, Y2) -> {X1, Y1, X2, Y2} | false
	when X1 :: integer(), Y1 :: integer(), X2 :: integer(), Y2 :: integer().
intersection(Rect, X1, Y1, X2, Y2) ->
	esdl2:intersect_rect_and_line(Rect, X1, Y1, X2, Y2).

-spec is_empty(rect()) -> boolean().
is_empty(#{w := W, h := H}) ->
	(W =< 0) orelse (H =< 0).

-spec is_equal(rect(), rect()) -> boolean().
is_equal(Rect1, Rect2) ->
	Rect1 =:= Rect2.

-spec is_in(point(), rect()) -> boolean().
is_in(#{x := PX, y := PY}, #{x := RX, y := RY, w := RW, h := RH}) ->
	(PX >= RX) andalso (PX < RX + RW)
		andalso (PY >= RY) andalso (PY < RY + RH).

-spec union(rect(), rect()) -> rect().
union(Rect1, Rect2) ->
	esdl2:union_rect(Rect1, Rect2).

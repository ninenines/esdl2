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

-module(sdl_surface).

-export([get_dimensions/1]).
-export([load/1]).

-opaque surface() :: <<>>.
-export_type([surface/0]).

-spec get_dimensions(surface()) -> {non_neg_integer(), non_neg_integer()}.
get_dimensions(Surface) ->
	esdl2:get_surface_dimensions(Surface).

-spec load(string()) -> {ok, surface()} | sdl:error().
load(Filename) ->
	esdl2:img_load(Filename),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

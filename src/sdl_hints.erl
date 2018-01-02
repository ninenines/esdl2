%% Copyright (c) 2015-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(sdl_hints).

-export([add_callback/3]).
-export([test_callback/3]).

%% The hint names are the same as the environment variable names
%% that SDL2 accepts.

-spec add_callback(string(), module(), atom()) -> ok.
add_callback(Hint, Module, Function) ->
	esdl2:add_hint_callback(Hint, Module, Function).

%% This callback can be used to test hints.
-spec test_callback(string(), undefined | string(), undefined | string()) -> ok.
test_callback(Hint, OldValue, NewValue) ->
	io:format("Hint ~p has value changed from ~p to ~p~n", [Hint, OldValue, NewValue]).

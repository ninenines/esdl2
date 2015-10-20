%% Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
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

-module(sdl_gl).

-export([create_context/1]).
-export([swap_window/1]).

-opaque context() :: <<>>.
-export_type([context/0]).

-spec create_context(sdl_window:window()) -> {ok, context()} | sdl:error().
create_context(Window) ->
	esdl2:gl_create_context(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec swap_window(sdl_window:window()) -> ok.
swap_window(Window) ->
	esdl2:gl_swap_window(Window).

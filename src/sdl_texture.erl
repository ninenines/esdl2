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

-module(sdl_texture).

-export([create_from_file/2]).
-export([create_from_surface/2]).
-export([get_alpha_mod/1]).
-export([get_blend_mode/1]).
-export([get_color_mod/1]).
-export([set_alpha_mod/2]).
-export([set_blend_mode/2]).

create_from_file(Renderer, Filename) ->
	{ok, Surface} = sdl_surface:load(Filename),
	create_from_surface(Renderer, Surface).

create_from_surface(Renderer, Surface) ->
	esdl2:create_texture_from_surface(Renderer, Surface),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

get_alpha_mod(Texture) ->
	esdl2:get_texture_alpha_mod(Texture),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Alpha} = Ret,
		Alpha
	end.

get_blend_mode(Texture) ->
	esdl2:get_texture_blend_mode(Texture),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Alpha} = Ret,
		Alpha
	end.

get_color_mod(Texture) ->
	esdl2:get_texture_color_mod(Texture),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Alpha} = Ret,
		Alpha
	end.

set_alpha_mod(Texture, Alpha) ->
	esdl2:set_texture_alpha_mod(Texture, Alpha),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

set_blend_mode(Texture, BlendMode) ->
	esdl2:set_texture_blend_mode(Texture, BlendMode),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

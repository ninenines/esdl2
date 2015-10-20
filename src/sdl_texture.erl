%% Copyright (c) 2014-2015, Loïc Hoguin <essen@ninenines.eu>
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
-export([set_color_mod/4]).

-opaque texture() :: <<>>.
-export_type([texture/0]).

-spec create_from_file(sdl_renderer:renderer(), string()) -> {ok, texture()} | sdl:error().
create_from_file(Renderer, Filename) ->
	{ok, Surface} = sdl_surface:load(Filename),
	create_from_surface(Renderer, Surface).

-spec create_from_surface(sdl_renderer:renderer(), sdl_surface:surface()) -> {ok, texture()} | sdl:error().
create_from_surface(Renderer, Surface) ->
	esdl2:create_texture_from_surface(Renderer, Surface),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_alpha_mod(texture()) -> byte().
get_alpha_mod(Texture) ->
	esdl2:get_texture_alpha_mod(Texture),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Alpha} = Ret,
		Alpha
	end.

-spec get_blend_mode(texture()) -> sdl_renderer:blend_mode().
get_blend_mode(Texture) ->
	esdl2:get_texture_blend_mode(Texture),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, BlendMode} = Ret,
		BlendMode
	end.

-spec get_color_mod(texture()) -> {byte(), byte(), byte()}.
get_color_mod(Texture) ->
	esdl2:get_texture_color_mod(Texture),
	receive {'_nif_thread_ret_', Ret} ->
		{ok, Color} = Ret,
		Color
	end.

-spec set_alpha_mod(texture(), byte()) -> ok | sdl:error().
set_alpha_mod(Texture, Alpha) ->
	esdl2:set_texture_alpha_mod(Texture, Alpha),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_blend_mode(texture(), sdl_renderer:blend_mode()) -> ok | sdl:error().
set_blend_mode(Texture, BlendMode) ->
	esdl2:set_texture_blend_mode(Texture, BlendMode),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_color_mod(texture(), byte(), byte(), byte()) -> ok | sdl:error().
set_color_mod(Texture, R, G, B) ->
	esdl2:set_texture_color_mod(Texture, R, G, B),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

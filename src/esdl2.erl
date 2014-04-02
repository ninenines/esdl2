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

-module(esdl2).

%% sdl
-export([init/1]).
-export([init_subsystem/1]).
-export([quit/0]).
-export([quit_subsystem/1]).
-export([set_main_ready/0]).
-export([was_init/1]).

%% sdl_clipboard
-export([get_clipboard_text/0]).
-export([has_clipboard_text/0]).
-export([set_clipboard_text/1]).

%% sdl_events
-export([poll_event/0]).

%% sdl_power
-export([get_power_info/0]).

%% sdl_renderer
-export([create_renderer/3]).
-export([render_clear/1]).
-export([render_copy/4]).
-export([render_present/1]).
-export([render_set_logical_size/3]).
-export([set_render_draw_color/5]).

%% sdl_surface
-export([img_load/1]).

%% sdl_texture
-export([create_texture_from_surface/2]).

%% sdl_version
-export([get_version/0]).
-export([get_revision/0]).

%% sdl_window
-export([create_window/6]).

%% @todo We probably want to accept an env variable or somthing for the location.
-on_load(on_load/0).
on_load() ->
	PrivDir = case code:priv_dir(?MODULE) of
		{error, _} ->
			AppPath = filename:dirname(filename:dirname(code:which(?MODULE))),
			filename:join(AppPath, "priv");
		Path ->
			Path
	end,
	erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

%% sdl

init(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

init_subsystem(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

quit() ->
	erlang:nif_error({not_loaded, ?MODULE}).

quit_subsystem(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_main_ready() ->
	erlang:nif_error({not_loaded, ?MODULE}).

was_init(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_clipboard

get_clipboard_text() ->
	erlang:nif_error({not_loaded, ?MODULE}).

has_clipboard_text() ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_clipboard_text(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_events

poll_event() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_power

get_power_info() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_renderer

create_renderer(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_clear(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_copy(_, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_present(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

render_set_logical_size(_, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

set_render_draw_color(_, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_surface

img_load(_) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_texture

create_texture_from_surface(_, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_version

get_version() ->
	erlang:nif_error({not_loaded, ?MODULE}).

get_revision() ->
	erlang:nif_error({not_loaded, ?MODULE}).

%% sdl_window

create_window(_, _, _, _, _, _) ->
	erlang:nif_error({not_loaded, ?MODULE}).

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

-module(sdl).

-export([start/0]).
-export([start/1]).
-export([stop/0]).
-export([stop_on_exit/0]).
-export([start_subsystems/1]).
-export([stop_subsystems/1]).
-export([is_started/1]).
-export([which_subsystems/0]).

-type error() :: {error, string()}.
-export_type([error/0]).

-type subsystem() :: timer | audio | video | joystick | haptic
	| game_controller | events | everything.

-spec start() -> ok | error().
start() ->
	start([]).

-spec start([subsystem()]) -> ok | {application_start_error, term()} | error().
start(Subsystems) ->
	case ensure_started() of
		ok ->
			esdl2:init(Subsystems),
			receive {'_nif_thread_ret_', Ret} -> Ret end;
		Error ->
			Error
	end.

ensure_started() ->
	case application:start(esdl2) of
		ok ->
			ok;
		{error, {already_started, esdl2}} ->
			ok;
		{error, Reason} ->
			{application_start_error, Reason}
	end.

-spec stop() -> ok.
stop() ->
	esdl2:quit().

-spec stop_on_exit() -> ok.
stop_on_exit() ->
	Self = self(),
	spawn_link(fun() ->
		process_flag(trap_exit, true),
		receive {'EXIT', Self, _} ->
			stop()
		end
	end),
	ok.

-spec start_subsystems([subsystem()]) -> ok | error().
start_subsystems(Subsystems) ->
	esdl2:init_subsystem(Subsystems),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec stop_subsystems([subsystem()]) -> ok.
stop_subsystems(Subsystems) ->
	esdl2:quit_subsystem(Subsystems).

-spec is_started(subsystem()) -> boolean().
is_started(Subsystem) ->
	esdl2:was_init([Subsystem]),
	receive {'_nif_thread_ret_', Ret} -> Ret =:= [Subsystem] end.

-spec which_subsystems() -> [subsystem()].
which_subsystems() ->
	esdl2:was_init([]),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

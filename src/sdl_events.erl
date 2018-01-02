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

-module(sdl_events).

-export([flush/1]).
-export([flush/2]).
-export([get/3]).
-export([has/1]).
-export([has/2]).
-export([peek/3]).
-export([poll/0]).
-export([pump/0]).

-type event_type() :: first
	| quit
	| app_terminating | app_low_memory | app_will_enter_background
	| app_did_enter_background | app_will_enter_foreground | app_did_enter_foreground
	| window | syswm
	| key_down | key_up | text_editing | text_input | keymap_changed
	| mouse_motion | mouse_down | mouse_up | mouse_wheel
	| joy_axis_motion | joy_ball_motion | joy_hat_motion
	| joy_button_down | joy_button_up | joy_device_added | joy_device_removed
	| controller_axis_motion | controller_button_down | controller_button_up
	| controller_device_added | controller_device_removed | controller_device_remapped
	| finger_down | finger_up | finger_motion
	| dollar_gesture | dollar_record | multi_gesture
	| clipboard_update
	| drop_file | drop_text | drop_begin | drop_complete
	| audio_device_added | audio_device_removed
	| render_targets_reset | render_device_reset
	| last.

-type window_event() :: #{type=>window, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), event=>sdl_window:window_event_type(),
	data1=>integer(), data2=>integer()}.

-type keyboard_event() :: #{type=>key_down | key_up, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), state=>released | pressed, repeat=>boolean(),
	scancode=>non_neg_integer(), sym=>non_neg_integer(), mod=>[sdl_keycode:keymod()]}.

-type mouse_motion_event() :: #{type=>mouse_motion, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), which=>touch | non_neg_integer(),
	x=>integer(), y=>integer(), xrel=>integer(), yrel=>integer()}.

-type mouse_button_event() :: #{type=>mouse_down | mouse_up, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), which=>touch | non_neg_integer(),
	button=>sdl_mouse:button(), state=>released | pressed,
	clicks=>non_neg_integer(), x=>integer(), y=>integer()}.

-type mouse_wheel_event() :: #{type=>mouse_wheel, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), which=>touch | non_neg_integer(),
	x=>integer(), y=>integer(), direction=>sdl_mouse:wheel_direction()}.

-type generic_event() :: #{
	type=> quit | app_terminating | app_low_memory | app_will_enter_background
		| app_did_enter_background | app_will_enter_foreground | app_did_enter_foreground
		| keymap_changed | clipboard_update | render_targets_reset | render_device_reset,
	timestamp=>non_neg_integer()}.

-type event() :: window_event() | keyboard_event()
	| mouse_motion_event() | mouse_button_event() | mouse_wheel_event()
	| generic_event().

-spec flush(event_type()) -> ok.
flush(Type) ->
	esdl2:flush_event(Type).

-spec flush(event_type(), event_type()) -> ok.
flush(MinType, MaxType) ->
	esdl2:flush_events(MinType, MaxType).

-spec get(non_neg_integer(), event_type(), event_type())
	-> {ok, [event()]} | sdl:error().
get(NumEvents, MinType, MaxType) ->
	esdl2:peep_events(get, NumEvents, MinType, MaxType),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec has(event_type()) -> boolean().
has(Type) ->
	esdl2:has_event(Type),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec has(event_type(), event_type()) -> boolean().
has(MinType, MaxType) ->
	esdl2:has_events(MinType, MaxType),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec peek(non_neg_integer(), event_type(), event_type())
	-> {ok, [event()]} | sdl:error().
peek(NumEvents, MinType, MaxType) ->
	esdl2:peep_events(peek, NumEvents, MinType, MaxType),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec poll() -> event() | false.
poll() ->
	esdl2:poll_event(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec pump() -> ok.
pump() ->
	esdl2:pump_events().

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

-module(sdl_events).

-export([poll/0]).

-type window_event_type() :: shown | hidden | exposed | moved | resized
	| size_changed | minimized | maximized | restored | enter | leave
	| focus_gained | focus_lost | close.
-type window_event() :: #{type=>window, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), event=>window_event_type(),
	data=>{integer(), integer()}}.

-type keymod() :: left_shift | right_shift | left_ctrl | right_ctrl
	| left_alt | right_alt | left_gui | right_gui | num | caps | mode.
-type key_event() :: #{type=>key_down | key_up, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), repeat=>boolean(), scancode=>non_neg_integer(),
	sym=>non_neg_integer(), mod=>[keymod()]}.

-type mouse_button() :: left | middle | right | x1 | x2 | non_neg_integer().
-type mouse_button_event() :: #{type=>mouse_down | mouse_up, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), which=>touch | non_neg_integer(),
	button=>mouse_button(), clicks=>non_neg_integer(), x=>integer(), y=>integer()}.

-type mouse_motion_event() :: #{type=>mouse_motion, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), which=>touch | non_neg_integer(),
	x=>integer(), y=>integer(), xrel=>integer(), yrel=>integer()}.

-type mouse_wheel_event() :: #{type=>mouse_wheel, timestamp=>non_neg_integer(),
	window_id=>non_neg_integer(), which=>touch | non_neg_integer(),
	x=>integer(), y=>integer()}.

-type quit_event() :: #{type=>quit, timestamp=>non_neg_integer()}.

-type event() :: window_event() | key_event() | mouse_button_event()
	| mouse_motion_event() | mouse_wheel_event() | quit_event().

-spec poll() -> event() | false.
poll() ->
	esdl2:poll_event(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

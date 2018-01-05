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

-module(sdl_keyboard).

-export([get_focused_window/0]).
-export([get_key_from_name/1]).
-export([get_key_from_scancode/1]).
-export([get_key_name/1]).
-export([get_mod_state/0]).
-export([get_scancode_from_key/1]).
-export([get_scancode_from_name/1]).
-export([get_scancode_name/1]).
-export([get_state/0]).
-export([has_screen_keyboard_support/0]).
-export([is_screen_keyboard_shown/1]).
-export([is_text_input_active/0]).
-export([set_mod_state/1]).
-export([set_text_input_rect/1]).
-export([start_text_input/0]).
-export([stop_text_input/0]).

-spec get_focused_window() -> sdl_window:window() | undefined.
get_focused_window() ->
	esdl2:get_keyboard_focus(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_key_from_name(binary()) -> non_neg_integer() | undefined.
get_key_from_name(Name) ->
	esdl2:get_key_from_name(Name).

-spec get_key_from_scancode(non_neg_integer()) -> non_neg_integer() | undefined.
get_key_from_scancode(Scancode) ->
	esdl2:get_key_from_scancode(Scancode).

-spec get_key_name(non_neg_integer()) -> binary().
get_key_name(Key) ->
	esdl2:get_key_name(Key),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_mod_state() -> [sdl_keycode:keymod()].
get_mod_state() ->
	esdl2:get_mod_state(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec get_scancode_from_key(non_neg_integer()) -> non_neg_integer() | undefined.
get_scancode_from_key(Key) ->
	esdl2:get_scancode_from_key(Key).

-spec get_scancode_from_name(binary()) -> non_neg_integer() | undefined.
get_scancode_from_name(Name) ->
	esdl2:get_scancode_from_name(Name).

-spec get_scancode_name(non_neg_integer()) -> binary().
get_scancode_name(Scancode) ->
	esdl2:get_scancode_name(Scancode).

-spec get_state() -> #{non_neg_integer() => boolean()}.
get_state() ->
	esdl2:get_keyboard_state(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec has_screen_keyboard_support() -> boolean().
has_screen_keyboard_support() ->
	esdl2:has_screen_keyboard_support(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec is_screen_keyboard_shown(sdl_window:window()) -> boolean().
is_screen_keyboard_shown(Window) ->
	esdl2:is_screen_keyboard_shown(Window),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec is_text_input_active() -> boolean().
is_text_input_active() ->
	esdl2:is_text_input_active(),
	receive {'_nif_thread_ret_', Ret} -> Ret end.

-spec set_mod_state([sdl_keycode:keymod()]) -> ok.
set_mod_state(Mod) ->
	esdl2:set_mod_state(Mod).

-spec set_text_input_rect(sdl_rect:rect()) -> ok.
set_text_input_rect(Rect) ->
	esdl2:set_text_input_rect(Rect).

-spec start_text_input() -> ok.
start_text_input() ->
	esdl2:start_text_input().

-spec stop_text_input() -> ok.
stop_text_input() ->
	esdl2:stop_text_input().

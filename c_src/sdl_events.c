// Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#include "esdl2.h"

#define EVENT_TYPE_ENUM(E) \
	E(window, SDL_WINDOWEVENT) \
	E(key_down, SDL_KEYDOWN) \
	E(key_up, SDL_KEYUP) \
	E(mouse_motion, SDL_MOUSEMOTION) \
	E(mouse_down, SDL_MOUSEBUTTONDOWN) \
	E(mouse_up, SDL_MOUSEBUTTONUP) \
	E(mouse_wheel, SDL_MOUSEWHEEL) \
	E(quit, SDL_QUIT)

NIF_ENUM_TO_ATOM_FUNCTION(event_type_to_atom, Uint32, EVENT_TYPE_ENUM)

#define WINDOW_EVENT_ENUM(E) \
	E(shown, SDL_WINDOWEVENT_SHOWN) \
	E(hidden, SDL_WINDOWEVENT_HIDDEN) \
	E(exposed, SDL_WINDOWEVENT_EXPOSED) \
	E(moved, SDL_WINDOWEVENT_MOVED) \
	E(resized, SDL_WINDOWEVENT_RESIZED) \
	E(size_changed, SDL_WINDOWEVENT_SIZE_CHANGED) \
	E(minimized, SDL_WINDOWEVENT_MINIMIZED) \
	E(maximized, SDL_WINDOWEVENT_MAXIMIZED) \
	E(restored, SDL_WINDOWEVENT_RESTORED) \
	E(enter, SDL_WINDOWEVENT_ENTER) \
	E(leave, SDL_WINDOWEVENT_LEAVE) \
	E(focus_gained, SDL_WINDOWEVENT_FOCUS_GAINED) \
	E(focus_lost, SDL_WINDOWEVENT_FOCUS_LOST) \
	E(close, SDL_WINDOWEVENT_CLOSE)

NIF_ENUM_TO_ATOM_FUNCTION(window_event_to_atom, Uint8, WINDOW_EVENT_ENUM)

#define KEYMOD_FLAGS(F) \
	F(left_shift, KMOD_LSHIFT) \
	F(right_shift, KMOD_RSHIFT) \
	F(left_ctrl, KMOD_LCTRL) \
	F(right_ctrl, KMOD_RCTRL) \
	F(left_alt, KMOD_LALT) \
	F(right_alt, KMOD_RALT) \
	F(left_gui, KMOD_LGUI) \
	F(right_gui, KMOD_RGUI) \
	F(num, KMOD_NUM) \
	F(caps, KMOD_CAPS) \
	F(mode, KMOD_MODE)

NIF_FLAGS_TO_LIST_FUNCTION(keymod_flags_to_list, Uint16, KEYMOD_FLAGS)

#define BUTTON_ENUM(E) \
	E(left, SDL_BUTTON_LEFT) \
	E(middle, SDL_BUTTON_MIDDLE) \
	E(right, SDL_BUTTON_RIGHT) \
	E(x1, SDL_BUTTON_X1) \
	E(x2, SDL_BUTTON_X2)

NIF_ENUM_TO_ATOM_FUNCTION(button_to_atom, Uint8, BUTTON_ENUM)

NIF_FUNCTION(poll_event)
{
	SDL_Event event;
	ERL_NIF_TERM map;

	if (SDL_PollEvent(&event) == 0)
		return atom_false;

	map = enif_make_new_map(env);

	// All events have a type and a timestamp.
	enif_make_map_put(env, map, atom_type,
		event_type_to_atom(event.type), &map);
	enif_make_map_put(env, map, atom_timestamp,
		enif_make_uint(env, event.common.timestamp), &map);

	// Some events have additional information.
	if (event.type == SDL_WINDOWEVENT) {
		enif_make_map_put(env, map, atom_window_id,
			enif_make_uint(env, event.window.windowID), &map);
		enif_make_map_put(env, map, atom_event,
			window_event_to_atom(event.window.event), &map);
		enif_make_map_put(env, map, atom_data,
			enif_make_tuple2(env,
				enif_make_int(env, event.window.data1),
				enif_make_int(env, event.window.data2)),
			&map);
	} else if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
		enif_make_map_put(env, map, atom_window_id,
			enif_make_uint(env, event.key.windowID), &map);
		// We don't pass the state as this information is redundant with the type.
		enif_make_map_put(env, map, atom_repeat,
			event.key.repeat == 0 ? atom_false : atom_true, &map);
		enif_make_map_put(env, map, atom_scancode,
			enif_make_uint(env, event.key.keysym.scancode), &map);
		enif_make_map_put(env, map, atom_sym,
			enif_make_uint(env, event.key.keysym.sym), &map);
		enif_make_map_put(env, map, atom_mod,
			keymod_flags_to_list(env, event.key.keysym.mod), &map);
	} else if (event.type == SDL_MOUSEMOTION) {
		enif_make_map_put(env, map, atom_window_id,
			enif_make_uint(env, event.motion.windowID), &map);
		enif_make_map_put(env, map, atom_which,
			(event.motion.which == SDL_TOUCH_MOUSEID)
				? atom_touch
				: enif_make_uint(env, event.motion.which),
			&map);
		// @todo We may want the state value here as it's a bitmask.
		// Question is how do we represent it to the Erlang code?
		enif_make_map_put(env, map, atom_x,
			enif_make_int(env, event.motion.x), &map);
		enif_make_map_put(env, map, atom_y,
			enif_make_int(env, event.motion.y), &map);
		enif_make_map_put(env, map, atom_xrel,
			enif_make_int(env, event.motion.xrel), &map);
		enif_make_map_put(env, map, atom_yrel,
			enif_make_int(env, event.motion.yrel), &map);
	} else if (event.type == SDL_MOUSEBUTTONDOWN || event.type == SDL_MOUSEBUTTONUP) {
		enif_make_map_put(env, map, atom_window_id,
			enif_make_uint(env, event.button.windowID), &map);
		enif_make_map_put(env, map, atom_which,
			(event.button.which == SDL_TOUCH_MOUSEID)
				? atom_touch
				: enif_make_uint(env, event.button.which),
			&map);
		enif_make_map_put(env, map, atom_button,
			(event.button.button <= SDL_BUTTON_X2)
				? button_to_atom(event.button.button)
				: enif_make_uint(env, event.button.button),
			&map);
		// We don't pass the state as this information is redundant with the type.
		// @todo SDL 2.0.2 clicks
		enif_make_map_put(env, map, atom_x,
			enif_make_int(env, event.button.x), &map);
		enif_make_map_put(env, map, atom_y,
			enif_make_int(env, event.button.y), &map);
	} else if (event.type == SDL_MOUSEWHEEL) {
		enif_make_map_put(env, map, atom_window_id,
			enif_make_uint(env, event.wheel.windowID), &map);
		enif_make_map_put(env, map, atom_which,
			(event.wheel.which == SDL_TOUCH_MOUSEID)
				? atom_touch
				: enif_make_uint(env, event.wheel.which),
			&map);
		enif_make_map_put(env, map, atom_x,
			enif_make_int(env, event.wheel.x), &map);
		enif_make_map_put(env, map, atom_y,
			enif_make_int(env, event.wheel.y), &map);
	}

	// @todo SDL_TextEditingEvent
	// @todo SDL_TextInputEvent
	// @todo SDL_JoyAxisEvent	
	// @todo SDL_JoyBallEvent	
	// @todo SDL_JoyHatEvent	
	// @todo SDL_JoyButtonEvent	
	// @todo SDL_JoyDeviceEvent	
	// @todo SDL_ControllerAxisEvent	
	// @todo SDL_ControllerButtonEvent	
	// @todo SDL_ControllerDeviceEvent	
	// @todo SDL_UserEvent	
	// @todo SDL_SysWMEvent	
	// @todo SDL_TouchFingerEvent	
	// @todo SDL_MultiGestureEvent	
	// @todo SDL_DollarGestureEvent	
	// @todo SDL_DropEvent	

	return map;
}

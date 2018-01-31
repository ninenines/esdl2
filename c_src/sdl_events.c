// Copyright (c) 2014-2018, Loïc Hoguin <essen@ninenines.eu>
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
	E(first, SDL_FIRSTEVENT) \
	E(quit, SDL_QUIT) \
	E(app_terminating, SDL_APP_TERMINATING) \
	E(app_low_memory, SDL_APP_LOWMEMORY) \
	E(app_will_enter_background, SDL_APP_WILLENTERBACKGROUND) \
	E(app_did_enter_background, SDL_APP_DIDENTERBACKGROUND) \
	E(app_will_enter_foreground, SDL_APP_WILLENTERFOREGROUND) \
	E(app_did_enter_foreground, SDL_APP_DIDENTERFOREGROUND) \
	E(window, SDL_WINDOWEVENT) \
	E(syswm, SDL_SYSWMEVENT) \
	E(key_down, SDL_KEYDOWN) \
	E(key_up, SDL_KEYUP) \
	E(text_editing, SDL_TEXTEDITING) \
	E(text_input, SDL_TEXTINPUT) \
	E(keymap_changed, SDL_KEYMAPCHANGED) \
	E(mouse_motion, SDL_MOUSEMOTION) \
	E(mouse_down, SDL_MOUSEBUTTONDOWN) \
	E(mouse_up, SDL_MOUSEBUTTONUP) \
	E(mouse_wheel, SDL_MOUSEWHEEL) \
	E(joy_axis_motion, SDL_JOYAXISMOTION) \
	E(joy_ball_motion, SDL_JOYBALLMOTION) \
	E(joy_hat_motion, SDL_JOYHATMOTION) \
	E(joy_button_down, SDL_JOYBUTTONDOWN) \
	E(joy_button_up, SDL_JOYBUTTONUP) \
	E(joy_device_added, SDL_JOYDEVICEADDED) \
	E(joy_device_removed, SDL_JOYDEVICEREMOVED) \
	E(controller_axis_motion, SDL_CONTROLLERAXISMOTION) \
	E(controller_button_down, SDL_CONTROLLERBUTTONDOWN) \
	E(controller_button_up, SDL_CONTROLLERBUTTONUP) \
	E(controller_device_added, SDL_CONTROLLERDEVICEADDED) \
	E(controller_device_removed, SDL_CONTROLLERDEVICEREMOVED) \
	E(controller_device_remapped, SDL_CONTROLLERDEVICEREMAPPED) \
	E(finger_down, SDL_FINGERDOWN) \
	E(finger_up, SDL_FINGERUP) \
	E(finger_motion, SDL_FINGERMOTION) \
	E(dollar_gesture, SDL_DOLLARGESTURE) \
	E(dollar_record, SDL_DOLLARRECORD) \
	E(multi_gesture, SDL_MULTIGESTURE) \
	E(clipboard_update, SDL_CLIPBOARDUPDATE) \
	E(drop_file, SDL_DROPFILE) \
	E(drop_text, SDL_DROPTEXT) \
	E(drop_begin, SDL_DROPBEGIN) \
	E(drop_complete, SDL_DROPCOMPLETE) \
	E(audio_device_added, SDL_AUDIODEVICEADDED) \
	E(audio_device_removed, SDL_AUDIODEVICEREMOVED) \
	E(render_targets_reset, SDL_RENDER_TARGETS_RESET) \
	E(render_device_reset, SDL_RENDER_DEVICE_RESET) \
	E(last, SDL_LASTEVENT)

static NIF_ENUM_TO_ATOM_FUNCTION(event_type_to_atom, Uint32, EVENT_TYPE_ENUM)
static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_event_type, Uint32, EVENT_TYPE_ENUM)

#define EVENT_ACTION_ENUM(E) \
	E(add, SDL_ADDEVENT) \
	E(peek, SDL_PEEKEVENT) \
	E(get, SDL_GETEVENT)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_event_action, SDL_eventaction, EVENT_ACTION_ENUM)

// Event conversion functions.

static ERL_NIF_TERM window_event_to_map(ErlNifEnv* env, SDL_Event* event, ERL_NIF_TERM map)
{
	enif_make_map_put(env, map, atom_window_id,
		enif_make_uint(env, event->window.windowID), &map);
	enif_make_map_put(env, map, atom_event,
		window_event_to_atom(event->window.event), &map);
	enif_make_map_put(env, map, atom_data1,
		enif_make_int(env, event->window.data1), &map);
	enif_make_map_put(env, map, atom_data2,
		enif_make_int(env, event->window.data2), &map);

	return map;
}

static ERL_NIF_TERM keyboard_event_to_map(ErlNifEnv* env, SDL_Event* event, ERL_NIF_TERM map)
{
	enif_make_map_put(env, map, atom_window_id,
		enif_make_uint(env, event->key.windowID), &map);
	enif_make_map_put(env, map, atom_state,
		event->key.state == SDL_RELEASED ? atom_released : atom_pressed, &map);
	enif_make_map_put(env, map, atom_repeat,
		event->key.repeat == 0 ? atom_false : atom_true, &map);
	enif_make_map_put(env, map, atom_scancode,
		enif_make_uint(env, event->key.keysym.scancode), &map);
	enif_make_map_put(env, map, atom_sym,
		enif_make_int(env, event->key.keysym.sym), &map);
	enif_make_map_put(env, map, atom_mod,
		keymod_flags_to_list(env, event->key.keysym.mod), &map);

	return map;
}

static ERL_NIF_TERM mouse_motion_event_to_map(ErlNifEnv* env, SDL_Event* event, ERL_NIF_TERM map)
{
	enif_make_map_put(env, map, atom_window_id,
		enif_make_uint(env, event->motion.windowID), &map);
	enif_make_map_put(env, map, atom_which,
		(event->motion.which == SDL_TOUCH_MOUSEID)
			? atom_touch
			: enif_make_uint(env, event->motion.which),
		&map);
	enif_make_map_put(env, map, atom_state,
		mouse_state_to_list(env, event->motion.state), &map);
	enif_make_map_put(env, map, atom_x,
		enif_make_int(env, event->motion.x), &map);
	enif_make_map_put(env, map, atom_y,
		enif_make_int(env, event->motion.y), &map);
	enif_make_map_put(env, map, atom_xrel,
		enif_make_int(env, event->motion.xrel), &map);
	enif_make_map_put(env, map, atom_yrel,
		enif_make_int(env, event->motion.yrel), &map);

	return map;
}

static ERL_NIF_TERM mouse_button_event_to_map(ErlNifEnv* env, SDL_Event* event, ERL_NIF_TERM map)
{
	enif_make_map_put(env, map, atom_window_id,
		enif_make_uint(env, event->button.windowID), &map);
	enif_make_map_put(env, map, atom_which,
		(event->button.which == SDL_TOUCH_MOUSEID)
			? atom_touch
			: enif_make_uint(env, event->button.which),
		&map);
	enif_make_map_put(env, map, atom_button,
		button_to_atom(event->button.button), &map);
	enif_make_map_put(env, map, atom_state,
		event->button.state == SDL_RELEASED ? atom_released : atom_pressed, &map);
	enif_make_map_put(env, map, atom_clicks,
		enif_make_uint(env, event->button.clicks), &map);
	enif_make_map_put(env, map, atom_x,
		enif_make_int(env, event->button.x), &map);
	enif_make_map_put(env, map, atom_y,
		enif_make_int(env, event->button.y), &map);

	return map;
}

static ERL_NIF_TERM mouse_wheel_event_to_map(ErlNifEnv* env, SDL_Event* event, ERL_NIF_TERM map)
{
	enif_make_map_put(env, map, atom_window_id,
		enif_make_uint(env, event->wheel.windowID), &map);
	enif_make_map_put(env, map, atom_which,
		(event->wheel.which == SDL_TOUCH_MOUSEID)
			? atom_touch
			: enif_make_uint(env, event->wheel.which),
		&map);
	enif_make_map_put(env, map, atom_x,
		enif_make_int(env, event->wheel.x), &map);
	enif_make_map_put(env, map, atom_y,
		enif_make_int(env, event->wheel.y), &map);
	enif_make_map_put(env, map, atom_direction,
		mousewheel_direction_to_atom(event->wheel.direction), &map);

	return map;
}

static ERL_NIF_TERM event_to_map(ErlNifEnv* env, SDL_Event* event)
{
	ERL_NIF_TERM map;

	map = enif_make_new_map(env);

	// All events have a type and a timestamp.

	enif_make_map_put(env, map, atom_type,
		event_type_to_atom(event->type), &map);
	enif_make_map_put(env, map, atom_timestamp,
		enif_make_uint(env, event->common.timestamp), &map);

	// The following event types have no additional fields:
	//
	// - SDL_QUIT
	// - SDL_APP_TERMINATING
	// - SDL_APP_LOWMEMORY
	// - SDL_APP_WILLENTERBACKGROUND
	// - SDL_APP_DIDENTERBACKGROUND
	// - SDL_APP_WILLENTERFOREGROUND
	// - SDL_APP_DIDENTERFOREGROUND
	// - SDL_KEYMAPCHANGED
	// - SDL_CLIPBOARDUPDATE
	// - SDL_RENDER_TARGETS_RESET
	// - SDL_RENDER_DEVICE_RESET

	switch (event->type) {
		case SDL_WINDOWEVENT:
			return window_event_to_map(env, event, map);

		// @todo SDL_SYSWMEVENT

		case SDL_KEYDOWN:
		case SDL_KEYUP:
			return keyboard_event_to_map(env, event, map);

		// @todo SDL_TEXTEDITING
		// @todo SDL_TEXTINPUT

		case SDL_MOUSEMOTION:
			return mouse_motion_event_to_map(env, event, map);

		case SDL_MOUSEBUTTONDOWN:
		case SDL_MOUSEBUTTONUP:
			return mouse_button_event_to_map(env, event, map);

		case SDL_MOUSEWHEEL:
			return mouse_wheel_event_to_map(env, event, map);

		// @todo SDL_JOYAXISMOTION
		// @todo SDL_JOYBALLMOTION
		// @todo SDL_JOYHATMOTION
		// @todo SDL_JOYBUTTONDOWN
		// @todo SDL_JOYBUTTONUP
		// @todo SDL_JOYDEVICEADDED
		// @todo SDL_JOYDEVICEREMOVED

		// @todo SDL_CONTROLLERAXISMOTION
		// @todo SDL_CONTROLLERBUTTONDOWN
		// @todo SDL_CONTROLLERBUTTONUP
		// @todo SDL_CONTROLLERDEVICEADDED
		// @todo SDL_CONTROLLERDEVICEREMOVED
		// @todo SDL_CONTROLLERDEVICEREMAPPED

		// @todo SDL_FINGERDOWN
		// @todo SDL_FINGERUP
		// @todo SDL_FINGERMOTION

		// @todo SDL_DOLLARGESTURE
		// @todo SDL_DOLLARRECORD
		// @todo SDL_MULTIGESTURE

		// @todo SDL_DROPFILE
		// @todo SDL_DROPTEXT
		// @todo SDL_DROPBEGIN
		// @todo SDL_DROPCOMPLETE

		// @todo SDL_AUDIODEVICEADDED
		// @todo SDL_AUDIODEVICEREMOVED
	}

	return map;
}

// flush_event

NIF_CAST_HANDLER(thread_flush_event)
{
	SDL_FlushEvent((long)args[0]);
}

NIF_FUNCTION(flush_event)
{
	Uint32 type;

	BADARG_IF(!atom_to_event_type(env, argv[0], &type));

	return nif_thread_cast(env, thread_flush_event, 1, type);
}

// flush_events

NIF_CAST_HANDLER(thread_flush_events)
{
	SDL_FlushEvents((long)args[0], (long)args[1]);
}

NIF_FUNCTION(flush_events)
{
	Uint32 minType, maxType;

	BADARG_IF(!atom_to_event_type(env, argv[0], &minType));
	BADARG_IF(!atom_to_event_type(env, argv[1], &maxType));

	return nif_thread_cast(env, thread_flush_events, 2, minType, maxType);
}

// has_event

NIF_CALL_HANDLER(thread_has_event)
{
	if (SDL_HasEvent((long)args[0]))
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(has_event)
{
	Uint32 type;

	BADARG_IF(!atom_to_event_type(env, argv[0], &type));

	return nif_thread_call(env, thread_has_event, 1, type);
}

// has_events

NIF_CALL_HANDLER(thread_has_events)
{
	if (SDL_HasEvents((long)args[0], (long)args[1]))
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(has_events)
{
	Uint32 minType, maxType;

	BADARG_IF(!atom_to_event_type(env, argv[0], &minType));
	BADARG_IF(!atom_to_event_type(env, argv[1], &maxType));

	return nif_thread_call(env, thread_has_events, 2, minType, maxType);
}

// peep_events
//
// @todo It is not currently possible to add events at the back of the queue.

NIF_CALL_HANDLER(thread_peep_events)
{
	SDL_Event* events;
	int i, numEvents;
	ERL_NIF_TERM list;

	events = enif_alloc(sizeof(SDL_Event) * (long)args[1]);

	numEvents = SDL_PeepEvents(events, (long)args[1],
		(long)args[0], (long)args[2], (long)args[3]);

	if (numEvents < 0)
		return sdl_error_tuple(env);

	list = enif_make_list(env, 0);

	for (i = 0; i < numEvents; i++)
		list = enif_make_list_cell(env, event_to_map(env, &events[i]), list);

	enif_free(events);

	return enif_make_tuple2(env, atom_ok, list);
}

NIF_FUNCTION(peep_events)
{
	SDL_eventaction action;
	int numEvents;
	Uint32 minType, maxType;

	BADARG_IF(enif_is_identical(atom_add, argv[0]));

	BADARG_IF(!atom_to_event_action(env, argv[0], &action));
	BADARG_IF(!enif_get_int(env, argv[1], &numEvents));
	BADARG_IF(!atom_to_event_type(env, argv[2], &minType));
	BADARG_IF(!atom_to_event_type(env, argv[3], &maxType));

	return nif_thread_call(env, thread_peep_events, 4,
		action, numEvents, minType, maxType);
}

// poll_event

NIF_CALL_HANDLER(thread_poll_event)
{
	SDL_Event event;

	if (SDL_PollEvent(&event) == 0)
		return atom_false;

	return event_to_map(env, &event);
}

NIF_FUNCTION(poll_event)
{
	return nif_thread_call(env, thread_poll_event, 0);
}

// pump_events

NIF_CAST_HANDLER(thread_pump_events)
{
	SDL_PumpEvents();
}

NIF_FUNCTION(pump_events)
{
	return nif_thread_cast(env, thread_pump_events, 0);
}

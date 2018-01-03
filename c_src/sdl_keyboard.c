// Copyright (c) 2015-2018, Loïc Hoguin <essen@ninenines.eu>
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

// get_key_from_name

NIF_FUNCTION(get_key_from_name)
{
	ErlNifBinary bin;
	char* name;
	SDL_Keycode key;

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	name = malloc(bin.size + 1);
	memcpy(name, bin.data, bin.size);
	name[bin.size] = '\0';

	key = SDL_GetKeyFromName(name);

	free(name);

	if (key == SDLK_UNKNOWN)
		return atom_undefined;

	return enif_make_int(env, key);
}

// get_key_from_scancode

NIF_FUNCTION(get_key_from_scancode)
{
	SDL_Scancode scancode;
	SDL_Keycode key;

	BADARG_IF(!enif_get_uint(env, argv[0], &scancode));

	key = SDL_GetKeyFromScancode(scancode);

	if (key == SDLK_UNKNOWN)
		return atom_undefined;

	return enif_make_int(env, key);
}

// get_key_name
//
// SDL_GetKeyName is not thread-safe as it stores the returned
// value in a static array in at least some cases. We use the
// NIF thread to avoid any issues.

NIF_CALL_HANDLER(thread_get_key_name)
{
	const char* name;
	ErlNifBinary bin;

	name = SDL_GetKeyName((long)args[0]);

	enif_alloc_binary(strlen(name), &bin);

	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

NIF_FUNCTION(get_key_name)
{
	SDL_Keycode key;

	BADARG_IF(!enif_get_int(env, argv[0], &key));

	return nif_thread_call(env, thread_get_key_name, 1, key);
}

// get_scancode_from_key

NIF_FUNCTION(get_scancode_from_key)
{
	SDL_Keycode key;
	SDL_Scancode scancode;

	BADARG_IF(!enif_get_int(env, argv[0], &key));

	scancode = SDL_GetScancodeFromKey(key);

	if (scancode == SDL_SCANCODE_UNKNOWN)
		return atom_undefined;

	return enif_make_uint(env, scancode);
}

// get_scancode_from_name

NIF_FUNCTION(get_scancode_from_name)
{
	ErlNifBinary bin;
	char* name;
	SDL_Scancode scancode;

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	name = malloc(bin.size + 1);
	memcpy(name, bin.data, bin.size);
	name[bin.size] = '\0';

	scancode = SDL_GetScancodeFromName(name);

	free(name);

	if (scancode == SDL_SCANCODE_UNKNOWN)
		return atom_undefined;

	return enif_make_uint(env, scancode);
}

// get_scancode_name
//
// The SDL_GetScancodeName function only ever points to static
// data and is therefore thread-safe, unlike SDL_GetKeyName.

NIF_FUNCTION(get_scancode_name)
{
	SDL_Scancode scancode;
	const char* name;
	ErlNifBinary bin;

	BADARG_IF(!enif_get_uint(env, argv[0], &scancode));

	name = SDL_GetScancodeName(scancode);

	enif_alloc_binary(strlen(name), &bin);

	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

// @todo get_keyboard_focus
// @todo get_keyboard_state

// get_mod_state

NIF_CALL_HANDLER(thread_get_mod_state)
{
	SDL_Keymod mod;

	mod = SDL_GetModState();

	return keymod_flags_to_list(env, mod);
}

NIF_FUNCTION(get_mod_state)
{
	return nif_thread_call(env, thread_get_mod_state, 0);
}

// @todo has_screen_keyboard_support
// @todo is_screen_keyboard_shown

// is_text_input_active

NIF_CALL_HANDLER(thread_is_text_input_active)
{
	if (SDL_IsTextInputActive())
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(is_text_input_active)
{
	return nif_thread_call(env, thread_is_text_input_active, 0);
}

// set_mod_state

NIF_CAST_HANDLER(thread_set_mod_state)
{
	SDL_SetModState((long)args[0]);
}

NIF_FUNCTION(set_mod_state)
{
	Uint16 mod;

	BADARG_IF(!keymod_list_to_flags(env, argv[0], &mod));

	return nif_thread_cast(env, thread_set_mod_state, 1, mod);
}

// @todo set_text_input_rect

// start_text_input

NIF_CAST_HANDLER(thread_start_text_input)
{
	SDL_StartTextInput();
}

NIF_FUNCTION(start_text_input)
{
	return nif_thread_cast(env, thread_start_text_input, 0);
}

// stop_text_input

NIF_CAST_HANDLER(thread_stop_text_input)
{
	SDL_StopTextInput();
}

NIF_FUNCTION(stop_text_input)
{
	return nif_thread_cast(env, thread_stop_text_input, 0);
}

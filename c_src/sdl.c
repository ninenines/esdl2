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

#define INIT_FLAGS(F) \
	F(timer, SDL_INIT_TIMER) \
	F(audio, SDL_INIT_AUDIO) \
	F(video, SDL_INIT_VIDEO) \
	F(joystick, SDL_INIT_JOYSTICK) \
	F(haptic, SDL_INIT_HAPTIC) \
	F(game_controller, SDL_INIT_GAMECONTROLLER) \
	F(events, SDL_INIT_EVENTS) \
	F(everything, SDL_INIT_EVERYTHING) \
	F(no_parachute, SDL_INIT_NOPARACHUTE)

NIF_LIST_TO_FLAGS_FUNCTION(list_to_init_flags, Uint32, INIT_FLAGS)

NIF_FUNCTION(init)
{
	Uint32 flags = 0;

	BADARG_IF(!list_to_init_flags(env, argv[0], &flags));

	if (SDL_Init(flags) == 0)
		return atom_ok;

	return sdl_error_tuple(env);
}

NIF_FUNCTION(init_subsystem)
{
	Uint32 flags = 0;

	BADARG_IF(!list_to_init_flags(env, argv[0], &flags));

	if (SDL_InitSubSystem(flags) == 0)
		return atom_ok;

	return sdl_error_tuple(env);
}

NIF_FUNCTION(quit)
{
	SDL_Quit();

	return atom_ok;
}

NIF_FUNCTION(quit_subsystem)
{
	Uint32 flags = 0;

	BADARG_IF(!list_to_init_flags(env, argv[0], &flags));

	SDL_QuitSubSystem(flags);

	return atom_ok;
}

NIF_FUNCTION(set_main_ready)
{
	SDL_SetMainReady();

	return atom_ok;
}

// @todo Implement the case where we want to receive a list of everything init.
NIF_FUNCTION(was_init)
{
	unsigned int length;
	Uint32 flags = 0;

	BADARG_IF(!enif_get_list_length(env, argv[0], &length));
	BADARG_IF(length == 0);
	BADARG_IF(!list_to_init_flags(env, argv[0], &flags));

	if (SDL_WasInit(flags))
		return atom_true;

	return atom_false;
}

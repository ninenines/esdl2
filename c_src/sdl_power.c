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

#define POWER_STATE_ENUM(E) \
	E(unknown, SDL_POWERSTATE_UNKNOWN) \
	E(on_battery, SDL_POWERSTATE_ON_BATTERY) \
	E(no_battery, SDL_POWERSTATE_NO_BATTERY) \
	E(charging, SDL_POWERSTATE_CHARGING) \
	E(charged, SDL_POWERSTATE_CHARGED)

NIF_ENUM_TO_ATOM_FUNCTION(power_state_to_atom, SDL_PowerState, POWER_STATE_ENUM)

// get_power_info

NIF_FUNCTION(get_power_info)
{
	SDL_PowerState state;
	int secs, pct;

	state = SDL_GetPowerInfo(&secs, &pct);

	return enif_make_tuple3(env,
		power_state_to_atom(state),
		enif_make_int(env, secs),
		enif_make_int(env, pct)
	);
}

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

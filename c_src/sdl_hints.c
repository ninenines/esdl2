// Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
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

// SDL_HintCallback

typedef struct esdl2_callback {
	char* module;
	char* function;
} esdl2_callback;

void esdl2_hint_callback(void* userdata, const char* name, const char* oldValue, const char* newValue)
{
	ErlNifEnv* env = enif_alloc_env();
	esdl2_callback* callback = (esdl2_callback*)userdata;
	ERL_NIF_TERM old;
	ERL_NIF_TERM new;

	if (oldValue == NULL)
		old = atom_undefined;
	else
		old = enif_make_string(env, oldValue, ERL_NIF_LATIN1);

	if (newValue == NULL)
		new = atom_undefined;
	else
		new = enif_make_string(env, newValue, ERL_NIF_LATIN1);

	enif_send(NULL, get_callback_process(), env,
		enif_make_tuple4(env, atom_callback,
			enif_make_atom(env, callback->module),
			enif_make_atom(env, callback->function),
			enif_make_list3(env,
				enif_make_string(env, name, ERL_NIF_LATIN1),
				old,
				new)));

	enif_free_env(env);
}

// add_hint_callback
// @todo We must free the userdata in SDL_DelHintCallback.

NIF_CAST_HANDLER(thread_add_hint_callback)
{
	esdl2_callback* callback = (esdl2_callback*)enif_alloc(sizeof(esdl2_callback));

	callback->module = args[1];
	callback->function = args[2];

	SDL_AddHintCallback(args[0], &esdl2_hint_callback, callback);

	enif_free(args[0]);
}

NIF_FUNCTION(add_hint_callback)
{
	unsigned int hint_len, module_len, function_len;
	char* hint;
	char* module;
	char* function;

	BADARG_IF(!enif_get_list_length(env, argv[0], &hint_len));
	BADARG_IF(!enif_get_atom_length(env, argv[1], &module_len, ERL_NIF_LATIN1));
	BADARG_IF(!enif_get_atom_length(env, argv[2], &function_len, ERL_NIF_LATIN1));

	hint = (char*)enif_alloc(hint_len + 1);
	if (!enif_get_string(env, argv[0], hint, hint_len + 1, ERL_NIF_LATIN1)) {
		enif_free(hint);
		return enif_make_badarg(env);
	}

	module = (char*)enif_alloc(module_len + 1);
	if (!enif_get_atom(env, argv[1], module, module_len + 1, ERL_NIF_LATIN1)) {
		enif_free(hint);
		enif_free(module);
		return enif_make_badarg(env);
	}

	function = (char*)enif_alloc(function_len + 1);
	if (!enif_get_atom(env, argv[2], function, function_len + 1, ERL_NIF_LATIN1)) {
		enif_free(hint);
		enif_free(module);
		enif_free(function);
		return enif_make_badarg(env);
	}

	return nif_thread_cast(env, thread_add_hint_callback, 3, hint, module, function);
}

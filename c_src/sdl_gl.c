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

void dtor_GLContext(ErlNifEnv* env, void* obj)
{
	SDL_GL_DeleteContext(NIF_RES_GET(GLContext, obj));
// @todo	enif_release_resource(NIF_RES_DEP(GLContext, obj));
}

// gl_create_context

NIF_CALL_HANDLER(thread_gl_create_context)
{
	SDL_GLContext context;
	ERL_NIF_TERM term;

	context = SDL_GL_CreateContext(args[0]);
	if (!context)
		return sdl_error_tuple(env);

	enif_keep_resource(args[0]);

// @todo	NIF_RES_TO_TERM_WITH_DEP(GLContext, context, term, args[0]);
	NIF_RES_TO_TERM(GLContext, context, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(gl_create_context)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_call(env, thread_gl_create_context, 1,
		NIF_RES_GET(Window, window_res));
}

// gl_swap_window

NIF_CAST_HANDLER(thread_gl_swap_window)
{
	SDL_GL_SwapWindow(args[0]);
}

NIF_FUNCTION(gl_swap_window)
{
	void* window_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Window, &window_res));

	return nif_thread_cast(env, thread_gl_swap_window, 1,
		NIF_RES_GET(Window, window_res));
}

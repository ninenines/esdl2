// Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

#define SYSTEM_CURSOR_ENUM(E) \
	E(arrow, SDL_SYSTEM_CURSOR_ARROW) \
	E(ibeam, SDL_SYSTEM_CURSOR_IBEAM) \
	E(wait, SDL_SYSTEM_CURSOR_WAIT) \
	E(crosshair, SDL_SYSTEM_CURSOR_CROSSHAIR) \
	E(wait_arrow, SDL_SYSTEM_CURSOR_WAITARROW) \
	E(size_nwse, SDL_SYSTEM_CURSOR_SIZENWSE) \
	E(size_nesw, SDL_SYSTEM_CURSOR_SIZENESW) \
	E(size_we, SDL_SYSTEM_CURSOR_SIZEWE) \
	E(size_ns, SDL_SYSTEM_CURSOR_SIZENS) \
	E(size_all, SDL_SYSTEM_CURSOR_SIZEALL) \
	E(no, SDL_SYSTEM_CURSOR_NO) \
	E(hand, SDL_SYSTEM_CURSOR_HAND)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_system_cursor, int, SYSTEM_CURSOR_ENUM)

void dtor_Cursor(ErlNifEnv* env, void* obj)
{
	SDL_Cursor* cursor = NIF_RES_GET(Cursor, obj);

	SDL_FreeCursor(cursor);
	esdl2_cursors_remove(cursor);
}

// create_cursor

NIF_CALL_HANDLER(thread_create_cursor)
{
	SDL_Cursor* cursor;
	obj_Cursor* res;
	ERL_NIF_TERM term;
	ErlNifBinary data, mask;

	enif_inspect_binary(args[0], (long)args[1], &data);
	enif_inspect_binary(args[0], (long)args[2], &mask);

	cursor = SDL_CreateCursor(data.data, mask.data,
		(long)args[3], (long)args[4], (long)args[5], (long)args[6]);

	enif_free_env(args[0]);

	if (!cursor)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Cursor, cursor, res, term);

	esdl2_cursors_insert(cursor, res);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(create_cursor)
{
	ErlNifEnv* bin_env;
	ERL_NIF_TERM data, mask;
	int w, h, hot_x, hot_y;

	BADARG_IF(!enif_is_binary(env, argv[0]));
	BADARG_IF(!enif_is_binary(env, argv[1]));
	BADARG_IF(!enif_get_int(env, argv[2], &w));
	BADARG_IF(!enif_get_int(env, argv[3], &h));
	BADARG_IF(!enif_get_int(env, argv[4], &hot_x));
	BADARG_IF(!enif_get_int(env, argv[5], &hot_y));

	// We copy the binaries to avoid copying their content.
	bin_env = enif_alloc_env();
	data = enif_make_copy(bin_env, argv[0]);
	mask = enif_make_copy(bin_env, argv[1]);

	return nif_thread_call(env, thread_create_cursor, 7,
		bin_env, data, mask, w, h, hot_x, hot_y);
}

// create_color_cursor

NIF_CALL_HANDLER(thread_create_color_cursor)
{
	SDL_Cursor* cursor;
	obj_Cursor* res;
	ERL_NIF_TERM term;

	cursor = SDL_CreateColorCursor(args[0], (long)args[1], (long)args[2]);

	if (!cursor)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Cursor, cursor, res, term);

	esdl2_cursors_insert(cursor, res);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(create_color_cursor)
{
	void* surface_res;
	int hot_x, hot_y;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Surface, &surface_res));
	BADARG_IF(!enif_get_int(env, argv[1], &hot_x));
	BADARG_IF(!enif_get_int(env, argv[2], &hot_y));

	return nif_thread_call(env, thread_create_color_cursor, 3,
		NIF_RES_GET(Surface, surface_res), hot_x, hot_y);
}

// create_system_cursor

NIF_CALL_HANDLER(thread_create_system_cursor)
{
	SDL_Cursor* cursor;
	obj_Cursor* res;
	ERL_NIF_TERM term;

	cursor = SDL_CreateSystemCursor((long)args[0]);

	if (!cursor)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Cursor, cursor, res, term);

	esdl2_cursors_insert(cursor, res);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(create_system_cursor)
{
	int id;

	BADARG_IF(!atom_to_system_cursor(env, argv[0], &id));

	return nif_thread_call(env, thread_create_system_cursor, 1, id);
}

// get_cursor

NIF_CALL_HANDLER(thread_get_cursor)
{
	SDL_Cursor* cursor;
	obj_Cursor* res;
	ERL_NIF_TERM term;

	cursor = SDL_GetCursor();

	// There is no mouse.
	if (!cursor)
		return atom_undefined;

	term = esdl2_cursors_find(env, cursor);

	if (!enif_is_identical(term, atom_undefined))
		return term;

	// We don't know this cursor. It's probably a system cursor.
	// We should insert it in the list to have the same reference
	// for it while it's active.

	NIF_RES_TO_PTR_AND_TERM(Cursor, cursor, res, term);

	esdl2_cursors_insert(cursor, res);

	return term;
}

NIF_FUNCTION(get_cursor)
{
	return nif_thread_call(env, thread_get_cursor, 0);
}

// get_default_cursor

NIF_CALL_HANDLER(thread_get_default_cursor)
{
	SDL_Cursor* cursor;
	obj_Cursor* res;
	ERL_NIF_TERM term;

	cursor = SDL_GetDefaultCursor();

	// There is no mouse.
	if (!cursor)
		return atom_undefined;

	term = esdl2_cursors_find(env, cursor);

	if (!enif_is_identical(term, atom_undefined))
		return term;

	// We don't know this cursor. It's probably a system cursor.
	// We should insert it in the list to have the same reference
	// for it while it's active.

	NIF_RES_TO_PTR_AND_TERM(Cursor, cursor, res, term);

	esdl2_cursors_insert(cursor, res);

	return term;
}

NIF_FUNCTION(get_default_cursor)
{
	return nif_thread_call(env, thread_get_default_cursor, 0);
}

// set_cursor

NIF_CAST_HANDLER(thread_set_cursor)
{
	SDL_SetCursor(args[0]);
}

NIF_FUNCTION(set_cursor)
{
	void* cursor_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Cursor, &cursor_res));

	return nif_thread_cast(env, thread_set_cursor, 1,
		NIF_RES_GET(Cursor, cursor_res));
}

// show_cursor

NIF_CALL_HANDLER(thread_show_cursor)
{
	return enif_make_int(env, SDL_ShowCursor((long)args[0]));
}

NIF_FUNCTION(show_cursor)
{
	int toggle;

	BADARG_IF(!enif_get_int(env, argv[0], &toggle));

	return nif_thread_call(env, thread_show_cursor, 1, toggle);
}

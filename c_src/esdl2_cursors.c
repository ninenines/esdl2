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
#include <sys/queue.h>

struct esdl2_cursor {
	LIST_ENTRY(esdl2_cursor) entries;

	SDL_Cursor* cursor;
	obj_Cursor* res;
};

static LIST_HEAD(esdl2_cursor_list, esdl2_cursor) cursors;

void esdl2_cursors_init()
{
	LIST_INIT(&cursors);
}

void esdl2_cursors_insert(SDL_Cursor* cursor, obj_Cursor* res)
{
	struct esdl2_cursor* item;

	item = malloc(sizeof(struct esdl2_cursor));
	item->cursor = cursor;
	item->res = res;

	LIST_INSERT_HEAD(&cursors, item, entries);
}

static struct esdl2_cursor* esdl2_cursors_find_entry(SDL_Cursor* cursor)
{
	struct esdl2_cursor* head;

	head = LIST_FIRST(&cursors);
	while (head != NULL) {
		if (head->cursor == cursor)
			return head;

		head = LIST_NEXT(head, entries);
	}

	return NULL;
}

ERL_NIF_TERM esdl2_cursors_find(ErlNifEnv* env, SDL_Cursor* cursor)
{
	struct esdl2_cursor* entry;
	ERL_NIF_TERM term;

	entry = esdl2_cursors_find_entry(cursor);

	if (entry == NULL)
		return atom_undefined;

	term = enif_make_resource(env, entry->res);

	return term;
}

void esdl2_cursors_remove(SDL_Cursor* cursor)
{
	struct esdl2_cursor* entry;

	entry = esdl2_cursors_find_entry(cursor);

	if (entry == NULL)
		return;

	LIST_REMOVE(entry, entries);
}

void esdl2_cursors_free()
{
	struct esdl2_cursor *head, *next;

	head = LIST_FIRST(&cursors);
	while (head != NULL) {
		next = LIST_NEXT(head, entries);
		free(head);
		head = next;
	}
}


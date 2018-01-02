// Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

struct esdl2_window {
	LIST_ENTRY(esdl2_window) entries;

	SDL_Window* window;
	obj_Window* res;
};

static LIST_HEAD(esdl2_window_list, esdl2_window) windows;

void esdl2_windows_init()
{
	LIST_INIT(&windows);
}

void esdl2_windows_insert(SDL_Window* window, obj_Window* res)
{
	struct esdl2_window* item;

	item = malloc(sizeof(struct esdl2_window));
	item->window = window;
	item->res = res;

	LIST_INSERT_HEAD(&windows, item, entries);
}

static struct esdl2_window* esdl2_windows_find_entry(SDL_Window* window)
{
	struct esdl2_window* head;

	head = LIST_FIRST(&windows);
	while (head != NULL) {
		if (head->window == window)
			return head;

		head = LIST_NEXT(head, entries);
	}

	return NULL;
}

ERL_NIF_TERM esdl2_windows_find(ErlNifEnv* env, SDL_Window* window)
{
	struct esdl2_window* entry;
	ERL_NIF_TERM term;

	entry = esdl2_windows_find_entry(window);

	if (entry == NULL)
		return atom_undefined;

	term = enif_make_resource(env, entry->res);

	return term;
}

void esdl2_windows_remove(SDL_Window* window)
{
	struct esdl2_window* entry;

	entry = esdl2_windows_find_entry(window);

	if (entry == NULL)
		return;

	LIST_REMOVE(entry, entries);
}

void esdl2_windows_free()
{
	struct esdl2_window *head, *next;

	head = LIST_FIRST(&windows);
	while (head != NULL) {
		next = LIST_NEXT(head, entries);
		free(head);
		head = next;
	}
}

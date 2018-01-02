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

struct esdl2_renderer {
	LIST_ENTRY(esdl2_renderer) entries;

	SDL_Renderer* renderer;
	obj_Renderer* res;
	obj_Window* window_res;
};

static LIST_HEAD(esdl2_renderer_list, esdl2_renderer) renderers;

void esdl2_renderers_init()
{
	LIST_INIT(&renderers);
}

void esdl2_renderers_insert(SDL_Renderer* renderer, obj_Renderer* res, obj_Window* window_res)
{
	struct esdl2_renderer* item;

	item = malloc(sizeof(struct esdl2_renderer));
	item->renderer = renderer;
	item->res = res;
	item->window_res = window_res;

	enif_keep_resource(window_res);

	LIST_INSERT_HEAD(&renderers, item, entries);
}

static struct esdl2_renderer* esdl2_renderers_find_entry(SDL_Renderer* renderer)
{
	struct esdl2_renderer* head;

	head = LIST_FIRST(&renderers);
	while (head != NULL) {
		if (head->renderer == renderer)
			return head;

		head = LIST_NEXT(head, entries);
	}

	return NULL;
}

ERL_NIF_TERM esdl2_renderers_find(ErlNifEnv* env, SDL_Renderer* renderer)
{
	struct esdl2_renderer* entry;
	ERL_NIF_TERM term;

	entry = esdl2_renderers_find_entry(renderer);

	if (entry == NULL)
		return atom_undefined;

	term = enif_make_resource(env, entry->res);

	return term;
}

void esdl2_renderers_remove(SDL_Renderer* renderer)
{
	struct esdl2_renderer* entry;

	entry = esdl2_renderers_find_entry(renderer);

	if (entry == NULL)
		return;

	enif_release_resource(entry->window_res);

	LIST_REMOVE(entry, entries);
}

void esdl2_renderers_free()
{
	struct esdl2_renderer *head, *next;

	head = LIST_FIRST(&renderers);
	while (head != NULL) {
		next = LIST_NEXT(head, entries);
		free(head);
		head = next;
	}
}


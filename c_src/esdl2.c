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
#include "nif_headers.h"
#include <sys/queue.h>

NIF_ATOMS(NIF_ATOM_DECL)
NIF_RESOURCES(NIF_RES_DECL)

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	NIF_ATOMS(NIF_ATOM_INIT)
	NIF_RESOURCES(NIF_RES_INIT)

	*priv_data = nif_create_main_thread("esdl2");

	return 0;
}

void unload(ErlNifEnv* env, void* priv_data)
{
	nif_destroy_main_thread(priv_data);
}

static ErlNifFunc nif_funcs[] = {
	NIF_FUNCTIONS(NIF_FUNCTION_ARRAY)
};

ERL_NIF_INIT(esdl2, nif_funcs, load, NULL, NULL, unload)

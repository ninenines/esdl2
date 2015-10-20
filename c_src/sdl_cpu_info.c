// Copyright (c) 2014-2015, Loïc Hoguin <essen@ninenines.eu>
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

// get_cpu_cache_line_size

NIF_FUNCTION(get_cpu_cache_line_size)
{
	return enif_make_int(env, SDL_GetCPUCacheLineSize());
}

// get_cpu_count

NIF_FUNCTION(get_cpu_count)
{
	return enif_make_int(env, SDL_GetCPUCount());
}

// get_system_ram

NIF_FUNCTION(get_system_ram)
{
	return enif_make_int(env, SDL_GetSystemRAM());
}

// has_3dnow

NIF_FUNCTION(has_3dnow)
{
	if (SDL_Has3DNow())
		return atom_true;

	return atom_false;
}

// has_avx

NIF_FUNCTION(has_avx)
{
	if (SDL_HasAVX())
		return atom_true;

	return atom_false;
}

// has_altivec

NIF_FUNCTION(has_altivec)
{
	if (SDL_HasAltiVec())
		return atom_true;

	return atom_false;
}

// has_mmx

NIF_FUNCTION(has_mmx)
{
	if (SDL_HasMMX())
		return atom_true;

	return atom_false;
}

// has_rdtsc

NIF_FUNCTION(has_rdtsc)
{
	if (SDL_HasRDTSC())
		return atom_true;

	return atom_false;
}

// has_sse

NIF_FUNCTION(has_sse)
{
	if (SDL_HasSSE())
		return atom_true;

	return atom_false;
}

// has_sse2

NIF_FUNCTION(has_sse2)
{
	if (SDL_HasSSE2())
		return atom_true;

	return atom_false;
}

// has_sse3

NIF_FUNCTION(has_sse3)
{
	if (SDL_HasSSE3())
		return atom_true;

	return atom_false;
}

// has_sse41

NIF_FUNCTION(has_sse41)
{
	if (SDL_HasSSE41())
		return atom_true;

	return atom_false;
}

// has_sse42

NIF_FUNCTION(has_sse42)
{
	if (SDL_HasSSE42())
		return atom_true;

	return atom_false;
}

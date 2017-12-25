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

#define BLEND_MODE_ENUM(E) \
	E(none, SDL_BLENDMODE_NONE) \
	E(blend, SDL_BLENDMODE_BLEND) \
	E(add, SDL_BLENDMODE_ADD) \
	E(mod, SDL_BLENDMODE_MOD) \
	E(invalid, SDL_BLENDMODE_INVALID)

NIF_ATOM_TO_ENUM_FUNCTION(atom_to_blend_mode, SDL_BlendMode, BLEND_MODE_ENUM)
NIF_ENUM_TO_ATOM_FUNCTION(blend_mode_to_atom, SDL_BlendMode, BLEND_MODE_ENUM)

#define BLEND_OPERATION_ENUM(E) \
	E(add, SDL_BLENDOPERATION_ADD) \
	E(substract, SDL_BLENDOPERATION_SUBTRACT) \
	E(rev_substract, SDL_BLENDOPERATION_REV_SUBTRACT) \
	E(minimum, SDL_BLENDOPERATION_MINIMUM) \
	E(maximum, SDL_BLENDOPERATION_MAXIMUM)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_blend_operation, SDL_BlendOperation, BLEND_OPERATION_ENUM)

#define BLEND_FACTOR_ENUM(E) \
	E(zero, SDL_BLENDFACTOR_ZERO) \
	E(one, SDL_BLENDFACTOR_ONE) \
	E(src_color, SDL_BLENDFACTOR_SRC_COLOR) \
	E(one_minus_src_color, SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR) \
	E(src_alpha, SDL_BLENDFACTOR_SRC_ALPHA) \
	E(one_minus_src_alpha, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA) \
	E(dst_color, SDL_BLENDFACTOR_DST_COLOR) \
	E(one_minus_dst_color, SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR) \
	E(dst_alpha, SDL_BLENDFACTOR_DST_ALPHA) \
	E(one_minus_dst_alpha, SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_blend_factor, SDL_BlendFactor, BLEND_FACTOR_ENUM)

// compose_custom_blend_mode

NIF_CALL_HANDLER(thread_compose_custom_blend_mode)
{
	SDL_BlendMode mode;
	ERL_NIF_TERM term;

	mode = SDL_ComposeCustomBlendMode(
		(long)args[0], (long)args[1], (long)args[2], 
		(long)args[3], (long)args[4], (long)args[5]);

	term = blend_mode_to_atom(mode);

	if (!enif_is_identical(term, atom_undefined))
		return term;

	return enif_make_int(env, mode);
}

NIF_FUNCTION(compose_custom_blend_mode)
{
	SDL_BlendFactor srcColorFactor, dstColorFactor, srcAlphaFactor, dstAlphaFactor;
	SDL_BlendOperation colorOp, alphaOp;

	BADARG_IF(!atom_to_blend_factor(env, argv[0], &srcColorFactor));
	BADARG_IF(!atom_to_blend_factor(env, argv[1], &dstColorFactor));
	BADARG_IF(!atom_to_blend_operation(env, argv[2], &colorOp));
	BADARG_IF(!atom_to_blend_factor(env, argv[3], &srcAlphaFactor));
	BADARG_IF(!atom_to_blend_factor(env, argv[4], &dstAlphaFactor));
	BADARG_IF(!atom_to_blend_operation(env, argv[5], &alphaOp));

	return nif_thread_call(env, thread_compose_custom_blend_mode, 6,
		srcColorFactor, dstColorFactor, colorOp,
		srcAlphaFactor, dstAlphaFactor, alphaOp);
}

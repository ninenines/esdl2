// Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

// @todo These operations should probably occur in the thread.
void dtor_Font(ErlNifEnv* env, void* obj)
{
	TTF_Font* font = NIF_RES_GET(Font, obj);

	TTF_CloseFont(font);
}

#define TTF_STYLE_FLAGS(F) \
	F(normal, TTF_STYLE_NORMAL) \
	F(bold, TTF_STYLE_BOLD) \
	F(italic, TTF_STYLE_ITALIC) \
	F(underline, TTF_STYLE_UNDERLINE) \
	F(strikethrough, TTF_STYLE_STRIKETHROUGH)

static NIF_LIST_TO_FLAGS_FUNCTION(list_to_ttf_style_flags, int, TTF_STYLE_FLAGS)
static NIF_FLAGS_TO_LIST_FUNCTION(ttf_style_flags_to_list, int, TTF_STYLE_FLAGS)

#define TTF_HINTING_ENUM(E) \
	E(normal, TTF_HINTING_NORMAL) \
	E(light, TTF_HINTING_LIGHT) \
	E(mono, TTF_HINTING_MONO) \
	E(none, TTF_HINTING_NONE)

static NIF_ATOM_TO_ENUM_FUNCTION(atom_to_ttf_hinting, int, TTF_HINTING_ENUM)
static NIF_ENUM_TO_ATOM_FUNCTION(ttf_hinting_to_atom, int, TTF_HINTING_ENUM)

// ttf_font_ascent

NIF_CALL_HANDLER(thread_ttf_font_ascent)
{
	return enif_make_int(env, TTF_FontAscent(args[0]));
}

NIF_FUNCTION(ttf_font_ascent)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_ascent, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_descent

NIF_CALL_HANDLER(thread_ttf_font_descent)
{
	return enif_make_int(env, TTF_FontDescent(args[0]));
}

NIF_FUNCTION(ttf_font_descent)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_descent, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_face_family_name

NIF_CALL_HANDLER(thread_ttf_font_face_family_name)
{
	const char* name;
	ErlNifBinary bin;

	name = TTF_FontFaceFamilyName(args[0]);

	if (!name)
		return atom_undefined;

	enif_alloc_binary(strlen(name), &bin);
	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

NIF_FUNCTION(ttf_font_face_family_name)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_face_family_name, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_face_is_fixed_width

NIF_CALL_HANDLER(thread_ttf_font_face_is_fixed_width)
{
	if (TTF_FontFaceIsFixedWidth(args[0]))
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(ttf_font_face_is_fixed_width)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_face_is_fixed_width, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_face_style_name

NIF_CALL_HANDLER(thread_ttf_font_face_style_name)
{
	const char* name;
	ErlNifBinary bin;

	name = TTF_FontFaceStyleName(args[0]);

	if (!name)
		return atom_undefined;

	enif_alloc_binary(strlen(name), &bin);
	memcpy(bin.data, name, bin.size);

	return enif_make_binary(env, &bin);
}

NIF_FUNCTION(ttf_font_face_style_name)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_face_style_name, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_faces

NIF_CALL_HANDLER(thread_ttf_font_faces)
{
	return enif_make_int64(env, TTF_FontFaces(args[0]));
}

NIF_FUNCTION(ttf_font_faces)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_faces, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_height

NIF_CALL_HANDLER(thread_ttf_font_height)
{
	return enif_make_int(env, TTF_FontHeight(args[0]));
}

NIF_FUNCTION(ttf_font_height)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_height, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_font_line_skip

NIF_CALL_HANDLER(thread_ttf_font_line_skip)
{
	return enif_make_int(env, TTF_FontLineSkip(args[0]));
}

NIF_FUNCTION(ttf_font_line_skip)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_font_line_skip, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_get_font_hinting

NIF_CALL_HANDLER(thread_ttf_get_font_hinting)
{
	return ttf_hinting_to_atom(TTF_GetFontHinting(args[0]));
}

NIF_FUNCTION(ttf_get_font_hinting)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_get_font_hinting, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_get_font_kerning

NIF_CALL_HANDLER(thread_ttf_get_font_kerning)
{
	if (TTF_GetFontKerning(args[0]))
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(ttf_get_font_kerning)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_get_font_kerning, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_get_font_outline

NIF_CALL_HANDLER(thread_ttf_get_font_outline)
{
	return enif_make_int(env, TTF_GetFontOutline(args[0]));
}

NIF_FUNCTION(ttf_get_font_outline)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_get_font_outline, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_get_font_style

NIF_CALL_HANDLER(thread_ttf_get_font_style)
{
	return ttf_style_flags_to_list(env, TTF_GetFontStyle(args[0]));
}

NIF_FUNCTION(ttf_get_font_style)
{
	void* font_res;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));

	return nif_thread_call(env, thread_ttf_get_font_style, 1,
		NIF_RES_GET(Font, font_res));
}

// ttf_init

NIF_CALL_HANDLER(thread_ttf_init)
{
	if (TTF_Init())
		return sdl_error_tuple(env);

	return atom_ok;
}

NIF_FUNCTION(ttf_init)
{
	return nif_thread_call(env, thread_ttf_init, 0);
}

// ttf_open_font

NIF_CALL_HANDLER(thread_ttf_open_font)
{
	TTF_Font* font;
	obj_Font* res;
	ERL_NIF_TERM term;

	font = TTF_OpenFont(args[0], (long)args[1]);

	enif_free(args[0]);

	if (!font)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Font, font, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_open_font)
{
	ErlNifBinary bin;
	char* filename;
	int ptsize;

	BADARG_IF(!enif_get_int(env, argv[1], &ptsize));

	// Getting the filename last to simplify the code due to memory allocation.

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	filename = enif_alloc(bin.size + 1);
	memcpy(filename, bin.data, bin.size);
	filename[bin.size] = '\0';

	return nif_thread_call(env, thread_ttf_open_font, 2,
		filename, ptsize);
}

// ttf_open_font_index

NIF_CALL_HANDLER(thread_ttf_open_font_index)
{
	TTF_Font* font;
	obj_Font* res;
	ERL_NIF_TERM term;

	font = TTF_OpenFontIndex(args[0], (long)args[1], (long)args[2]);

	enif_free(args[0]);

	if (!font)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Font, font, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_open_font_index)
{
	ErlNifBinary bin;
	char* filename;
	int ptsize;
	long index;

	BADARG_IF(!enif_get_int(env, argv[1], &ptsize));
	BADARG_IF(!enif_get_int64(env, argv[2], &index));

	// Getting the filename last to simplify the code due to memory allocation.

	BADARG_IF(!enif_inspect_binary(env, argv[0], &bin));

	filename = enif_alloc(bin.size + 1);
	memcpy(filename, bin.data, bin.size);
	filename[bin.size] = '\0';

	return nif_thread_call(env, thread_ttf_open_font_index, 3,
		filename, ptsize, index);
}

// ttf_quit

NIF_CAST_HANDLER(thread_ttf_quit)
{
	TTF_Quit();
}

NIF_FUNCTION(ttf_quit)
{
	return nif_thread_cast(env, thread_ttf_quit, 0);
}

// ttf_render_utf8_blended

NIF_CALL_HANDLER(thread_ttf_render_utf8_blended)
{
	SDL_Surface* surface;
	obj_Surface* res;
	ERL_NIF_TERM term;

	surface = TTF_RenderUTF8_Blended(args[0], args[1], *(SDL_Color*)args[2]);

	enif_free(args[1]);
	enif_free(args[2]);

	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Surface, surface, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_render_utf8_blended)
{
	void* font_res;
	ErlNifBinary bin;
	char* text;
	SDL_Color* fg;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_inspect_binary(env, argv[1], &bin));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	fg = enif_alloc(sizeof(SDL_Color));
	if (!map_to_color(env, argv[2], fg)) {
		enif_free(text);
		enif_free(fg);

		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_ttf_render_utf8_blended, 3,
		NIF_RES_GET(Font, font_res), text, fg);
}

// ttf_render_utf8_blended_wrapped

NIF_CALL_HANDLER(thread_ttf_render_utf8_blended_wrapped)
{
	SDL_Surface* surface;
	obj_Surface* res;
	ERL_NIF_TERM term;

	surface = TTF_RenderUTF8_Blended_Wrapped(args[0], args[1],
		*(SDL_Color*)args[2], (long)args[3]);

	enif_free(args[1]);
	enif_free(args[2]);

	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Surface, surface, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_render_utf8_blended_wrapped)
{
	void* font_res;
	ErlNifBinary bin;
	char* text;
	SDL_Color* fg;
	Uint32 wrap_length;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_inspect_binary(env, argv[1], &bin));
	BADARG_IF(!enif_get_uint(env, argv[3], &wrap_length));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	fg = enif_alloc(sizeof(SDL_Color));
	if (!map_to_color(env, argv[2], fg)) {
		enif_free(text);
		enif_free(fg);

		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_ttf_render_utf8_blended_wrapped, 4,
		NIF_RES_GET(Font, font_res), text, fg, wrap_length);
}

// ttf_render_utf8_shaded

NIF_CALL_HANDLER(thread_ttf_render_utf8_shaded)
{
	SDL_Surface* surface;
	obj_Surface* res;
	ERL_NIF_TERM term;

	surface = TTF_RenderUTF8_Shaded(args[0], args[1],
		*(SDL_Color*)args[2], *(SDL_Color*)args[3]);

	enif_free(args[1]);
	enif_free(args[2]);
	enif_free(args[3]);

	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Surface, surface, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_render_utf8_shaded)
{
	void* font_res;
	ErlNifBinary bin;
	char* text;
	SDL_Color *fg, *bg;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_inspect_binary(env, argv[1], &bin));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	fg = enif_alloc(sizeof(SDL_Color));
	if (!map_to_color(env, argv[2], fg)) {
		enif_free(text);
		enif_free(fg);

		return enif_make_badarg(env);
	}

	bg = enif_alloc(sizeof(SDL_Color));
	if (!map_to_color(env, argv[3], bg)) {
		enif_free(text);
		enif_free(fg);
		enif_free(bg);

		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_ttf_render_utf8_shaded, 4,
		NIF_RES_GET(Font, font_res), text, fg, bg);
}

// ttf_render_utf8_solid

NIF_CALL_HANDLER(thread_ttf_render_utf8_solid)
{
	SDL_Surface* surface;
	obj_Surface* res;
	ERL_NIF_TERM term;

	surface = TTF_RenderUTF8_Solid(args[0], args[1], *(SDL_Color*)args[2]);

	enif_free(args[1]);
	enif_free(args[2]);

	if (!surface)
		return sdl_error_tuple(env);

	NIF_RES_TO_PTR_AND_TERM(Surface, surface, res, term);

	return enif_make_tuple2(env,
		atom_ok,
		term
	);
}

NIF_FUNCTION(ttf_render_utf8_solid)
{
	void* font_res;
	ErlNifBinary bin;
	char* text;
	SDL_Color* fg;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_inspect_binary(env, argv[1], &bin));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	fg = enif_alloc(sizeof(SDL_Color));
	if (!map_to_color(env, argv[2], fg)) {
		enif_free(text);
		enif_free(fg);

		return enif_make_badarg(env);
	}

	return nif_thread_call(env, thread_ttf_render_utf8_solid, 3,
		NIF_RES_GET(Font, font_res), text, fg);
}

// ttf_set_font_hinting

NIF_CAST_HANDLER(thread_ttf_set_font_hinting)
{
	TTF_SetFontHinting(args[0], (long)args[1]);
}

NIF_FUNCTION(ttf_set_font_hinting)
{
	void* font_res;
	int hinting;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!atom_to_ttf_hinting(env, argv[1], &hinting));

	return nif_thread_cast(env, thread_ttf_set_font_hinting, 2,
		NIF_RES_GET(Font, font_res), hinting);
}

// ttf_set_font_kerning

NIF_CAST_HANDLER(thread_ttf_set_font_kerning)
{
	TTF_SetFontKerning(args[0], (long)args[1]);
}

NIF_FUNCTION(ttf_set_font_kerning)
{
	void* font_res;
	SDL_bool b;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!atom_to_bool(env, argv[1], &b));

	return nif_thread_cast(env, thread_ttf_set_font_kerning, 2,
		NIF_RES_GET(Font, font_res), b);
}

// ttf_set_font_outline

NIF_CAST_HANDLER(thread_ttf_set_font_outline)
{
	TTF_SetFontOutline(args[0], (long)args[1]);
}

NIF_FUNCTION(ttf_set_font_outline)
{
	void* font_res;
	int outline;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_get_int(env, argv[1], &outline));

	return nif_thread_cast(env, thread_ttf_set_font_outline, 2,
		NIF_RES_GET(Font, font_res), outline);
}

// ttf_set_font_style

NIF_CAST_HANDLER(thread_ttf_set_font_style)
{
	TTF_SetFontStyle(args[0], (long)args[1]);
}

NIF_FUNCTION(ttf_set_font_style)
{
	void* font_res;
	int style = 0;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!list_to_ttf_style_flags(env, argv[1], &style));

	return nif_thread_cast(env, thread_ttf_set_font_style, 2,
		NIF_RES_GET(Font, font_res), style);
}

// ttf_size_utf8

NIF_CALL_HANDLER(thread_ttf_size_utf8)
{
	int error, w, h;

	error = TTF_SizeUTF8(args[0], args[1], &w, &h);

	enif_free(args[1]);

	if (error)
		return sdl_error_tuple(env);

	return enif_make_tuple3(env,
		atom_ok,
		enif_make_int(env, w),
		enif_make_int(env, h)
	);
}

NIF_FUNCTION(ttf_size_utf8)
{
	void* font_res;
	ErlNifBinary bin;
	char* text;

	BADARG_IF(!enif_get_resource(env, argv[0], res_Font, &font_res));
	BADARG_IF(!enif_inspect_binary(env, argv[1], &bin));

	text = enif_alloc(bin.size + 1);
	memcpy(text, bin.data, bin.size);
	text[bin.size] = '\0';

	return nif_thread_call(env, thread_ttf_size_utf8, 2,
		NIF_RES_GET(Font, font_res), text);
}

// ttf_was_init

NIF_CALL_HANDLER(thread_ttf_was_init)
{
	if (TTF_WasInit())
		return atom_true;

	return atom_false;
}

NIF_FUNCTION(ttf_was_init)
{
	return nif_thread_call(env, thread_ttf_was_init, 0);
}

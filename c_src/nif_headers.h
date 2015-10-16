 
#ifndef __NIF_HEADER_H__
#define __NIF_HEADER_H__


ERL_NIF_TERM blend_mode_to_atom  (int id);
ERL_NIF_TERM atom_to_blend_mode( ErlNifEnv *e, ERL_NIF_TERM t, SDL_BlendMode *id);
ERL_NIF_TERM atom_to_bool(ErlNifEnv *e, ERL_NIF_TERM t, SDL_bool *b );

void nif_destroy_main_thread(void* void_st);

#endif

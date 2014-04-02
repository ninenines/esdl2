%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(sdl_cpu_info).

-export([get_cpu_cache_line_size/0]).
-export([get_cpu_count/0]).
-export([get_system_ram/0]).
-export([has_3dnow/0]).
-export([has_avx/0]).
-export([has_altivec/0]).
-export([has_mmx/0]).
-export([has_rdtsc/0]).
-export([has_sse/0]).
-export([has_sse2/0]).
-export([has_sse3/0]).
-export([has_sse41/0]).
-export([has_sse42/0]).

get_cpu_cache_line_size() ->
	esdl2:get_cpu_cache_line_size().

get_cpu_count() ->
	esdl2:get_cpu_count().

get_system_ram() ->
	esdl2:get_system_ram().

has_3dnow() ->
	esdl2:has_3dnow().

has_avx() ->
	esdl2:has_avx().

has_altivec() ->
	esdl2:has_altivec().

has_mmx() ->
	esdl2:has_mmx().

has_rdtsc() ->
	esdl2:has_rdtsc().

has_sse() ->
	esdl2:has_sse().

has_sse2() ->
	esdl2:has_sse2().

has_sse3() ->
	esdl2:has_sse3().

has_sse41() ->
	esdl2:has_sse41().

has_sse42() ->
	esdl2:has_sse42().

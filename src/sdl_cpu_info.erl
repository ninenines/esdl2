%% Copyright (c) 2014-2018, Loïc Hoguin <essen@ninenines.eu>
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
-export([has_altivec/0]).
-export([has_avx/0]).
-export([has_avx2/0]).
-export([has_mmx/0]).
-export([has_neon/0]).
-export([has_rdtsc/0]).
-export([has_sse/0]).
-export([has_sse2/0]).
-export([has_sse3/0]).
-export([has_sse41/0]).
-export([has_sse42/0]).

-spec get_cpu_cache_line_size() -> non_neg_integer().
get_cpu_cache_line_size() ->
	esdl2:get_cpu_cache_line_size().

-spec get_cpu_count() -> non_neg_integer().
get_cpu_count() ->
	esdl2:get_cpu_count().

-spec get_system_ram() -> non_neg_integer().
get_system_ram() ->
	esdl2:get_system_ram().

-spec has_3dnow() -> boolean().
has_3dnow() ->
	esdl2:has_3dnow().

-spec has_altivec() -> boolean().
has_altivec() ->
	esdl2:has_altivec().

-spec has_avx() -> boolean().
has_avx() ->
	esdl2:has_avx().

-spec has_avx2() -> boolean().
has_avx2() ->
	esdl2:has_avx2().

-spec has_mmx() -> boolean().
has_mmx() ->
	esdl2:has_mmx().

-spec has_neon() -> boolean().
has_neon() ->
	esdl2:has_neon().

-spec has_rdtsc() -> boolean().
has_rdtsc() ->
	esdl2:has_rdtsc().

-spec has_sse() -> boolean().
has_sse() ->
	esdl2:has_sse().

-spec has_sse2() -> boolean().
has_sse2() ->
	esdl2:has_sse2().

-spec has_sse3() -> boolean().
has_sse3() ->
	esdl2:has_sse3().

-spec has_sse41() -> boolean().
has_sse41() ->
	esdl2:has_sse41().

-spec has_sse42() -> boolean().
has_sse42() ->
	esdl2:has_sse42().

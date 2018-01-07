%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(sdl_pixels).

-type pixel_format() :: unknown
	| index1lsb | index1msb | index4lsb | index4msb | index8
	| rgb332 | rgb444 | rgb555 | bgr555
	| argb4444 | rgba4444 | abgr4444 | bgra4444
	| argb1555 | rgba5551 | abgr1555 | bgra5551
	| rgb565 | bgr565 | rgb24 | bgr24
	| rgb888 | rgbx8888 | bgr888 | bgrx8888
	| argb8888 | rgba8888 | abgr8888 | bgra8888
	| argb2101010
	| rgba32 | argb32 | bgra32 | abgr32
	| yv12 | iyuv | yuy2 | uyvy | yvyu | nv12 | nv21.
-export_type([pixel_format/0]).

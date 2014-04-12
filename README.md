ESDL2
=====

SDL2 Erlang NIFs

SDL provides cross-platform low level access to graphics, audio, keyboard, mouseand joystick hardware via OpenGL and Direct3D. This is an Erlang binding. http://www.libsdl.org/

Status
------

Week-end project. Work in progress.
 
 Building
 --------
 
 *  Erlang 17.0+ is required
 *  SDL 2.0.3+ is required (`http://www.libsdl.org/download-2.0.php`)
 *  SDL\_image is required (`https://www.libsdl.org/projects/SDL_image/`)
 *  No support for UTF-8 strings, only Latin-1

Tip: configure SDL\_image with --disable-webp for not having to install libwebp.

Build using `make`.

Ideas
-----
The following ideas need to be investigated:

 *  We may benefit from the reference receive optimization when doing calls
 *  We may want a way to pipeline draw operations

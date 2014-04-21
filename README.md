ESDL2
=====

SDL2 Erlang NIFs

SDL provides cross-platform low level access to graphics, audio, keyboard, mouseand joystick hardware via OpenGL and Direct3D. This is an Erlang binding to SDL2. http://www.libsdl.org/

Note: 'ESDL' used to be Erlang's graphics kit, using SDL 1, before it was replaced by wxWidgets. This is a different implementation, for a new version of SDL.

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

Build using make:

    make

Examples
--------

Samples in examples/

 * Bullet Engine - rains bullets danmaku style
 * Hello World - minimal visual: show a graphic
 * Rose - minimal animation: draw a pixel line
 * Dot - minimal animation: show a circling dot

Build and run using make from the project root.

    make bullet
    make hello
    make rose
    make dot

Ideas
-----
The following ideas need to be investigated:

 *  We may benefit from the reference receive optimization when doing calls
 *  We may want a way to pipeline draw operations

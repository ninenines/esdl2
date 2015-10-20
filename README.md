ESDL2
=====

SDL2 Erlang NIF.

Status
------

Week-end project. Work in progress.

The following limitations apply:

 *  Erlang 17.0+ is required
 *  SDL 2.0.3+ is required
 *  No support for UTF-8 strings, only Latin-1

The following ideas need to be investigated:

 *  We may benefit from the reference receive optimization when doing calls
 *  We may want a way to pipeline draw operations
 *  Using HiPE mutable bytearrays for "void* pixels" types

The following tasks remain to be done:

* http://hg.libsdl.org/SDL/file/default/include/SDL_hints.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_error.h
  * Anything that can produce errors will have an automatic call to SDL_GetError.
  * I am not sure we need any direct access to those functions.

* http://hg.libsdl.org/SDL/file/default/include/SDL_log.h
  * We already have logging in Erlang. Not sure we need this.

* http://hg.libsdl.org/SDL/file/default/include/SDL_assert.h
  * I am not sure we need this, and we can't use the assert macro directly anyway.

* http://hg.libsdl.org/SDL/file/default/include/SDL_version.h
  * It might be useful to provide access to some of the macros as functions.
  * The SDL_GetRevisionNumber function needs to be implemented.
  * It might be interesting to use the macros to conditionally implement some features,
    this way we could be compatible with all versions >= 2.0.0.

* http://hg.libsdl.org/SDL/file/default/include/SDL_video.h
  * The SDL_DisplayMode structure and related functions need to be implemented:
    * SDL_GetNumDisplayModes()
    * SDL_GetDisplayMode()
    * SDL_GetDesktopDisplayMode()
    * SDL_GetCurrentDisplayMode()
    * SDL_GetClosestDisplayMode()
    * SDL_SetWindowDisplayMode()
    * SDL_GetWindowDisplayMode()
  * Most other display functions are also not implemented:
    * SDL_GetNumVideoDisplays()
	* SDL_GetDisplayName()
	* SDL_GetDisplayBounds()
	* SDL_GetDisplayDPI()
  * Some window functions need to be implemented:
    * SDL_CreateWindowFrom()
	* SDL_GetWindowData()
	* SDL_SetWindowData()
    * SDL_GetWindowPixelFormat()
	* SDL_GetWindowFromID()
	* SDL_GetWindowSurface()
	* SDL_UpdateWindowSurface()
	* SDL_UpdateWindowSurfaceRects()
	* SDL_GetGrabbedWindow()
	* SDL_SetWindowGammaRamp()
	* SDL_GetWindowGammaRamp()
  * There is an SDL_WINDOW_MOUSE_CAPTURE window flag we need to implement.
  * There are two window pos flags, related to multi monitors, that we don't support:
    * SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)
	* SDL_WINDOWPOS_CENTERED_DISPLAY(X)
  * We do not support OpenGL at all yet:
    * SDL_GLContext
	* SDL_GLattr
	* SDL_GLprofile
	* SDL_GLcontextFlag
	* SDL_GLcontextReleaseFlag
	* SDL_GL_LoadLibrary()
	* SDL_GL_GetProcAddress()
	* SDL_GL_UnloadLibrary()
	* SDL_GL_ExtensionSupported()
	* SDL_GL_ResetAttributes()
	* SDL_GL_SetAttribute()
	* SDL_GL_GetAttribute()
	* SDL_GL_MakeCurrent()
	* SDL_GL_GetCurrentWindow()
	* SDL_GL_GetCurrentContext()
	* SDL_GL_GetDrawableSize()
	* SDL_GL_SetSwapInterval()
	* SDL_GL_GetSwapInterval()
  * We do not support any of the video driver related functions:
    * SDL_GetNumVideoDrivers()
	* SDL_GetVideoDriver()
	* SDL_VideoInit()
	* SDL_VideoQuit()
	* SDL_GetCurrentVideoDriver()
  * Flags and functions relating to SDL_HitTest are not implemented:
    * SDL_HitTestResult
	* SDL_HitTest() callback
	* SDL_SetWindowHitTest()
  * Screensaver related functions are not implemented:
    * SDL_IsScreenSaverEnabled()
	* SDL_EnableScreenSaver()
	* SDL_DisableScreenSaver()

* http://hg.libsdl.org/SDL/file/default/include/SDL_render.h
  * The SDL_RendererInfo structure and related functions are not implemented:
    * SDL_GetRenderDriverInfo()
	* SDL_GetRendererInfo()
  * The SDL_TextureAccess enum is not implemented.
  * The SDL_TextureModulate enum is not implemented.
  * A number of other functions are not implemented:
    * SDL_CreateSoftwareRenderer()
	* SDL_GetRenderer()
	* SDL_CreateTexture()
	* SDL_QueryTexture()
	* SDL_UpdateTexture()
	* SDL_UpdateYUVTexture()
	* SDL_LockTexture()
	* SDL_UnlockTexture()
	* SDL_SetRenderTarget()
	* SDL_GetRenderTarget()
	* SDL_RenderIsClipEnabled()
	* SDL_RenderReadPixels()
  * We do not support OpenGL at all yet:
    * SDL_GL_BindTexture()
	* SDL_GL_UnbindTexture()

* http://hg.libsdl.org/SDL/file/default/include/SDL_pixels.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_rect.h
  * We need to implement everything, though we already have points and rects as maps elsewhere.

* http://hg.libsdl.org/SDL/file/default/include/SDL_surface.h
  * We need to implement everything except SDL_FreeSurface().

* http://hg.libsdl.org/SDL/file/default/include/SDL_syswm.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_events.h
  * The following event types need to be implemented:
    * SDL_APP_TERMINATING (iOS and Android)
	* SDL_APP_LOWMEMORY (iOS and Android)
	* SDL_APP_WILLENTERBACKGROUND (iOS and Android)
	* SDL_APP_DIDENTERBACKGROUND (iOS and Android)
	* SDL_APP_WILLENTERFOREGROUND (iOS and Android)
	* SDL_APP_DIDENTERFOREGROUND (iOS and Android)
	* SDL_SYSWMEVENT (SDL_SysWMEvent, SDL_SysWMmsg)
	* SDL_TEXTEDITING (SDL_TEXTEDITINGEVENT_TEXT_SIZE, SDL_TextEditingEvent)
	* SDL_TEXTINPUT (SDL_TEXTINPUTEVENT_TEXT_SIZE, SDL_TextInputEvent)
	* SDL_JOYAXISMOTION (SDL_JoyAxisEvent)
	* SDL_JOYBALLMOTION (SDL_JoyBallEvent)
	* SDL_JOYHATMOTION (SDL_JoyHatEvent)
	* SDL_JOYBUTTONDOWN (SDL_JoyButtonEvent)
	* SDL_JOYBUTTONUP (SDL_JoyButtonEvent)
	* SDL_JOYDEVICEADDED (SDL_JoyDeviceEvent)
	* SDL_JOYDEVICEREMOVED (SDL_JoyDeviceEvent)
	* SDL_CONTROLLERAXISMOTION (SDL_ControllerAxisEvent)
	* SDL_CONTROLLERBUTTONDOWN (SDL_ControllerButtonEvent)
	* SDL_CONTROLLERBUTTONUP (SDL_ControllerButtonEvent)
	* SDL_CONTROLLERDEVICEADDED (SDL_ControllerDeviceEvent)
	* SDL_CONTROLLERDEVICEREMOVED (SDL_ControllerDeviceEvent)
	* SDL_CONTROLLERDEVICEREMAPPED (SDL_ControllerDeviceEvent)
	* SDL_FINGERDOWN (SDL_TouchFingerEvent)
	* SDL_FINGERUP (SDL_TouchFingerEvent)
	* SDL_FINGERMOTION (SDL_TouchFingerEvent)
	* SDL_DOLLARGESTURE (SDL_DollarGestureEvent)
	* SDL_DOLLARRECORD (SDL_DollarGestureEvent)
	* SDL_MULTIGESTURE (SDL_MultiGestureEvent)
	* SDL_CLIPBOARDUPDATE
	* SDL_DROPFILE (SDL_DropEvent)
	* SDL_AUDIODEVICEADDED (SDL_AudioDeviceEvent)
	* SDL_AUDIODEVICEREMOVED (SDL_AudioDeviceEvent)
	* SDL_RENDER_TARGETS_RESET
	* SDL_RENDER_DEVICE_RESET
	* User defined events, from SDL_USEREVENT to SDL_LASTEVENT (SDL_UserEvent)
  * We currently do not handle the SDL_MouseMotionEvent.state value
  * We currently do not handle the SDL_MouseWheelEvent.direction value (SDL >= 2.0.4, unreleased yet)
  * We need to implement the following functions:
    * SDL_PumpEvents()
	* SDL_PeepEvents()
	* SDL_HasEvent()
	* SDL_HasEvents()
	* SDL_FlushEvent()
	* SDL_FlushEvents()
	* SDL_WaitEvent()
	* SDL_WaitEventTimeout()
	* SDL_PushEvent()
	* SDL_SetEventFilter()
	* SDL_GetEventFilter()
	* SDL_AddEventWatch()
	* SDL_DelEventWatch()
	* SDL_FilterEvents()
	* SDL_EventState()
	* SDL_RegisterEvents()

* http://hg.libsdl.org/SDL/file/default/include/SDL_keyboard.h
  * We need to implement the following functions:
  	* SDL_GetKeyboardFocus()
	* SDL_GetKeyboardState()
	* SDL_GetModState()
	* SDL_SetModState()
	* SDL_GetKeyFromScancode()
	* SDL_GetScancodeFromKey()
	* SDL_GetScancodeName()
	* SDL_GetScancodeFromName()
	* SDL_GetKeyName()
	* SDL_GetKeyFromName()
	* SDL_SetTextInputRect()
	* SDL_HasScreenKeyboardSupport()
	* SDL_IsScreenKeyboardShown()

* http://hg.libsdl.org/SDL/file/default/include/SDL_keycode.h
  * We probably should make it easy to identify keycodes and scancodes.

* http://hg.libsdl.org/SDL/file/default/include/SDL_scancode.h
  * We probably should make it easy to identify keycodes and scancodes.

* http://hg.libsdl.org/SDL/file/default/include/SDL_mouse.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_joystick.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_gamecontroller.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_haptic.h
  * We need to implement everything.

* http://hg.libsdl.org/SDL/file/default/include/SDL_audio.h
  * We need to implement everything.
  * We might want to implement SDL_mixer first.

* http://hg.libsdl.org/SDL/file/default/include/SDL_thread.h
  * We don't really need this for Erlang, do we?

* http://hg.libsdl.org/SDL/file/default/include/SDL_mutex.h
  * We don't really need this for Erlang, do we?

* http://hg.libsdl.org/SDL/file/default/include/SDL_atomic.h
  * We don't really need this for Erlang, do we?

* http://hg.libsdl.org/SDL/file/default/include/SDL_timer.h
  * We need to implement everything.
  * It's not the most useful to have, but doesn't hurt to have it.

* http://hg.libsdl.org/SDL/file/default/include/SDL_rwops.h
  * We need to implement everything.
  * It's currently unclear if and how this should be implemented.

* http://hg.libsdl.org/SDL/file/default/include/SDL_loadso.h
  * We don't really need this for Erlang, do we?

* http://hg.libsdl.org/SDL/file/default/include/SDL_platform.h
  * We need to implement the one function in there. Can always be useful.

* http://hg.libsdl.org/SDL/file/default/include/SDL_cpuinfo.h
  * The function SDL_HasAVX2() needs to be implemented. (SDL >= 2.0.4, unreleased yet)

* http://hg.libsdl.org/SDL/file/default/include/SDL_endian.h
  * We need to implement everything.
  * It's currently unclear if and how this should be implemented.

* http://hg.libsdl.org/SDL/file/default/include/SDL_bits.h
  * We need to implement everything.
  * It's currently unclear if and how this should be implemented.

* http://hg.libsdl.org/SDL/file/default/include/SDL_system.h
  * We need to implement everything.
  * It's currently unclear if and how this should be implemented.

* http://hg.libsdl.org/SDL/file/default/include/SDL_stdinc.h
  * Nothing except support for SDL_bool was implemented.
  * There might be useful bits and pieces in there, in particular the math or iconv functions.

* SDL_image, SDL_mixer, ... also need to be investigated and implemented.

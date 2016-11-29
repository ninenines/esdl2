{application, esdl2, [
	{description, "SDL2 Erlang NIF."},
	{vsn, "0.1.0"},
	{modules, ['esdl2','esdl2_app','esdl2_callbacks','esdl2_sup','sdl','sdl_clipboard','sdl_cpu_info','sdl_events','sdl_filesystem','sdl_gl','sdl_hints','sdl_keyboard','sdl_power','sdl_renderer','sdl_surface','sdl_texture','sdl_version','sdl_window']},
	{registered, [esdl2_sup]},
	{applications, [kernel,stdlib]},
	{mod, {esdl2_app, []}}
]}.
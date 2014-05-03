%% Minimal animation example: rose line art.
%% Build and run from project root: make rose
%% Feel free to copy and reuse as you wish.

-module(planets).
-export([run/0]).

run() ->
	spawn_opt(fun init/0, [{scheduler, 0}]).

init() ->
	%% setup sdl, window and renderer
	ok = sdl:start([video]),
	ok = sdl:stop_on_exit(),
	{ok, Window} = sdl_window:create("Planets", 10, 10, 500, 500, []),
	{ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated, present_vsync]),
	%% clear screen (once ever in this example)
	ok = sdl_renderer:set_draw_color(Renderer, 0,0,0,0),
	ok = sdl_renderer:clear(Renderer),
	%% start animaton
	loop(#{window=>Window, renderer=>Renderer, t=>os:timestamp()}).

%% animation loop
loop(State) ->
	events_loop(),
	render(State),
	loop(State).

%% check for termination
events_loop() ->
	case sdl_events:poll() of
		false -> ok;
		#{type:=quit} -> terminate();
		_ -> events_loop()
	end.

%% render one frame
render(#{renderer:=Renderer, t:=TT}=State) ->
	T = timer:now_diff(os:timestamp(), TT) / 20000,
	ok = sdl_renderer:set_draw_color(Renderer, 0,0,0,0),
	ok = sdl_renderer:clear(Renderer),
	%% draw one dot (it is not cleared next frame)
	draw_star(Renderer, T), 
	draw_planet(Renderer,  7, 200, 450, 600, 100, T), 
	draw_planet(Renderer,  3, 100, 280, 300, 30, T), 
	draw_planet(Renderer,  3, 100, 900, 300, 30, T+70), 
	draw_planet(Renderer,  5, 200, 290, 200, 30, T+100), 
	draw_planet(Renderer, 12, 300, 650, 700, 300, T), 
	%% render
	ok = sdl_renderer:present(Renderer).

draw_star(Renderer, T) ->
	ok = sdl_renderer:set_draw_color(Renderer, 255, 255, 255, 255),
	ok = sdl_renderer:draw_points(Renderer, circle(100, 250, 50, 120, T, 1)), 
	ok = sdl_renderer:draw_points(Renderer, circle(100, 250, 53, 30, -T, 1)), 
	ok = sdl_renderer:draw_points(Renderer, circle(100, 250, 45, 300, T/100, 1)).


draw_planet(Renderer, W, N, R, D, V, T) ->
	X = 100 + erlang:round(R * math:cos(T/D)), 
	Y = 250 + erlang:round(R * math:sin(T/D) / 5), 
	ok = sdl_renderer:set_draw_color(Renderer, 100, 100, 100, 255),
        ok = sdl_renderer:draw_points(Renderer, circle(X, Y, W, N, T/V, 1.25)),
	ok = sdl_renderer:set_draw_color(Renderer, 255, 255, 255, 255),
        ok = sdl_renderer:draw_points(Renderer, circle(X, Y, W, N, T/V, 1)).

%% T effects a slow ant crawl of the dashed circle line when N is small enough
%% in relation to R. 
%% S gives a breathing effect of the size, scaling the entire radius.
circle(X, Y, R0, N, T, S) ->
	R = S * (R0 + math:cos(T/20)), %% radius
	D = math:pi() * 2 / N,
	[ #{x => X + erlang:round(R * math:cos((TT+T/30)/D)), 
            y => Y + erlang:round(R * math:sin((TT+T/30)/D))}
	||  TT <- lists:seq(1, N)]. 

terminate() ->
	init:stop(),
	exit(normal).

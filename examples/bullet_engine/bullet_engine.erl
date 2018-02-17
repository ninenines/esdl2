%% This is an example. Feel free to copy and reuse as you wish.

-module(bullet_engine).
-export([run/0]).

run() ->
	spawn(fun init/0).

init() ->
	ok = sdl:start([video]),
	ok = sdl:stop_on_exit(),
	{ok, Window} = sdl_window:create(<<"Hello SDL">>, 10, 10, 500, 600, []),
	{ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated, present_vsync]),
	ok = sdl_renderer:set_logical_size(Renderer, 500 bsl 16, 600 bsl 16),
	{ok, Texture} = sdl_texture:create_from_file(Renderer, "bullet.png"),
	loop(#{window=>Window, renderer=>Renderer, texture=>Texture,
		scene=>init_scene()}).

loop(State=#{scene:=Scene}) ->
	events_loop(),
	Scene2 = update_scene(Scene, []),
	State2 = State#{scene:=Scene2},
	render(State2),
	loop(State2).

events_loop() ->
	case sdl_events:poll() of
		false -> ok;
		#{type:=quit} -> terminate();
		_ -> events_loop()
	end.

render(#{renderer:=Renderer, texture:=Texture, scene:=Scene}) ->
	ok = sdl_renderer:clear(Renderer),
	_ = [sdl_renderer:copy(Renderer, Texture, undefined, Bullet)
		|| Bullet = #{t:=Type} <- Scene, Type =/= invisible],
	ok = sdl_renderer:present(Renderer).

terminate() ->
	init:stop(),
	exit(normal).

%% Demo scene.

init_scene() ->
	[new_invisible(#{x=>250 bsl 16, y=>300 bsl 16, w=>0, h=>0, actions=>[
		%% Part 1.
		{var, w, 31},
		{var, x, 0},
		{loop, 30, [
			{loop, 12, [
				{fire, [
					{set, speed, 2 bsl 16},
					{set, dir, x}
				]},
				{var, x, '+=', 30}
			]},
			{wait, w},
			{var, w, '+=', -1}
		]},

		%% Part 2.
		{var, y, 31},
		{var, z, -3},
		{loop, 2, [
			{loop, 21, [
				{loop, 18, [
					{fire, [
						{set, speed, 2 bsl 16},
						{set, dir, x}
					]},
					{var, x, '+=', y},
					{wait, 1}
				]},
				{var, y, '+=', z}
			]},
			{var, z, '*=', -1}
		]},

		%% Part 3.
		{var, i, 45},
		{var, n, 4},
		{loop, 4, [
			{wait, 60},
			{fire, [
				{set, w, 64 bsl 16},
				{set, h, 64 bsl 16},
				{set, dir, i},
				{set, speed, 2 bsl 16},
				{loop, 10, [
					{fire, [
						{var, j, 180},
						{var, j, '+=', i},
						{set, dir, j},
						{set, speed, 10 bsl 16}
					]},
					{wait, 6}
				]},
				{set, speed, 0},
				{var, w, 60},
				{var, w, '*=', n},
				{wait, w},
				{var, d, 90},
				{var, n, '+=', -4},
				{var, n, '*=', -1},
				{var, d, '*=', n},
				{var, d, '+=', -225},
				{set, dir, d},
				{set, speed, 3 bsl 16},
				{loop, 120, [
					{var, d, '+=', 3},
					{set, dir, d},
					{wait, 1}
				]},
				{loop, 120, [
					{fire, []},
					{loop, 3, [
						{var, d, '+=', 3},
						{set, dir, d},
						{wait, 1}
					]}
				]},
				{loop, 120, [
					{fire, []},
					{var, d2, 180},
					{var, d2, '+=', d},
					{fire, [
						{set, dir, d2}
					]},
					{loop, 3, [
						{var, d, '+=', 3},
						{set, dir, d},
						{wait, 1}
					]}
				]},
				{loop, 120, [
					{fire, []},
					{var, d2, 180},
					{var, d2, '+=', d},
					{fire, [
						{set, dir, d2}
					]},
					{loop, 2, [
						{var, d, '+=', 3},
						{set, dir, d},
						{wait, 1}
					]}
				]},
				{loop, 120, [
					{fire, []},
					{var, d2, 180},
					{var, d2, '+=', d},
					{fire, [
						{set, dir, d2}
					]},
					{var, d, '+=', 3},
					{set, dir, d},
					{wait, 1}
				]},
				{wait, 240}
			]},
			{var, i, '+=', 90},
			{var, n, '+=', -1}
		]},

		%% Wait for the scene to finish, then stop the VM.
		{wait, 2640},
		init_stop
	]})].

update_scene([], Acc) ->
	lists:flatten(Acc);
%% We avoid float arithmetic where possible.
%% For Pi we use the approximate fraction 103993/33102.
%% We may also want to do an integer cosine and sine.
update_scene([Bullet = #{x:=X, y:=Y, w:=W, h:=H, dir:=Dir, speed:=Speed,
		wait:=Wait, actions:=Actions}|Tail], Acc) ->
	A = (103993 * (Dir - 90)) / (33102 * 180),
	X2 = X + round(Speed * math:cos(A)),
	Y2 = Y + round(Speed * math:sin(A)),
	if
		Wait > 0 ->
			update_scene(Tail, [Bullet#{x:=X2, y:=Y2, wait:=Wait - 1}|Acc]);
		X2 > 500 bsl 16; X2 < -W; Y2 > 600 bsl 16; Y2 < -H ->
			update_scene(Tail, Acc);
		true ->
			New = update_bullet(Bullet#{x:=X2, y:=Y2}, Actions, []),
			update_scene(Tail, [New|Acc])
	end.

%% Bullet engine.
%%
%% The scene is (500 bsl 16)x(600 bsl 16) rendered as 500x600. We avoid floats for
%% performance reasons so everything only goes up to 3 decimals, which is perfectly
%% fine anyway.
%%
%% Execution is done frame by frame for simplicity, relying on vsync. Another
%% advantage of doing this is that we can very easy record a replay based on user
%% input, although we don't have any in this small demo.

new_invisible(Parent) ->
	Parent#{t=>invisible, w=>0, h=>0, dir=>0, speed=>0,
		wait=>0, vars=>#{}}.

new_bullet(Parent=#{x:=X, y:=Y, w:=W, h:=H}, Actions) ->
	Parent#{t=>bullet, x=>X + (W div 2) - (8 bsl 16), y=>Y + (H div 2) - (8 bsl 16),
		w=>16 bsl 16, h=>16 bsl 16, wait=>0, actions=>Actions}.

update_bullet(Bullet, [], Acc) ->
	[Bullet#{actions:=[]}|Acc];
%% Stop the VM.
update_bullet(Bullet, [init_stop|_], Acc) ->
	init:stop(),
	update_bullet(Bullet, [], Acc);
%% Manipulate variables.
update_bullet(Bullet=#{vars:=Vars}, [{var, V, Value}|Tail], Acc) ->
	update_bullet(Bullet#{vars:=maps:put(V, Value, Vars)}, Tail, Acc);
update_bullet(Bullet=#{vars:=Vars}, [{var, V, '+=', W}|Tail], Acc) when is_atom(W) ->
	update_bullet(Bullet#{vars:=maps:put(V, maps:get(V, Vars) + maps:get(W, Vars), Vars)}, Tail, Acc);
update_bullet(Bullet=#{vars:=Vars}, [{var, V, '+=', W}|Tail], Acc) ->
	update_bullet(Bullet#{vars:=maps:put(V, maps:get(V, Vars) + W, Vars)}, Tail, Acc);
update_bullet(Bullet=#{vars:=Vars}, [{var, V, '*=', W}|Tail], Acc) when is_atom(W) ->
	update_bullet(Bullet#{vars:=maps:put(V, maps:get(V, Vars) * maps:get(W, Vars), Vars)}, Tail, Acc);
update_bullet(Bullet=#{vars:=Vars}, [{var, V, '*=', W}|Tail], Acc) ->
	update_bullet(Bullet#{vars:=maps:put(V, maps:get(V, Vars) * W, Vars)}, Tail, Acc);
%% Loop actions.
%%
%% We only unroll one iteration at a time to avoid wasting resources.
update_bullet(Bullet, [{loop, 1, Actions}|Tail], Acc) ->
	update_bullet(Bullet, Actions ++ Tail, Acc);
update_bullet(Bullet, [{loop, N, Actions}|Tail], Acc) ->
	update_bullet(Bullet, Actions ++ [{loop, N - 1, Actions}|Tail], Acc);
%% Wait a few frames.
update_bullet(Bullet=#{vars:=Vars}, [{wait, V}|Tail], Acc) when is_atom(V) ->
	[Bullet#{wait:=maps:get(V, Vars), actions:=Tail}|Acc];
update_bullet(Bullet, [{wait, N}|Tail], Acc) ->
	[Bullet#{wait:=N, actions:=Tail}|Acc];
%% Fire a new bullet.
update_bullet(Bullet, [{fire, Actions}|Tail], Acc) ->
	update_bullet(Bullet, Tail, [new_bullet(Bullet, Actions)|Acc]);
%% Set bullet values directly.
update_bullet(Bullet=#{vars:=Vars}, [{set, Key, V}|Tail], Acc) when is_atom(V) ->
	update_bullet(maps:put(Key, maps:get(V, Vars), Bullet), Tail, Acc);
update_bullet(Bullet=#{x:=X, y:=Y, w:=W, h:=H}, [{set, Key, Value}|Tail], Acc) ->
	%% We need to reposition the bullet if the size changes,
	%% are the bullet position is its top left corner.
	case Key of
		w -> update_bullet(maps:put(Key, Value, Bullet#{x:=X + ((W - Value) div 2)}), Tail, Acc);
		h -> update_bullet(maps:put(Key, Value, Bullet#{y:=Y + ((H - Value) div 2)}), Tail, Acc);
		_ -> update_bullet(maps:put(Key, Value, Bullet), Tail, Acc)
	end.

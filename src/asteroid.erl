%% @author alexei
%% @doc @todo Add description to asteroid.


-module(asteroid).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0, loop/3, line_process/3, read_file/1]).

run() ->
	Asteroids = read_file("asteroids.txt"),
	VList = lists:map(fun(A) -> {A, check_for_visibility(A, Asteroids, Asteroids, 0)} end, Asteroids),
%%	io:format("~p~n", [VList]),
	R = lists:foldr(fun({_, C} = E, {_, AC} = Acc) -> if C > AC -> E; true -> Acc end end, {{0,0},0}, VList),
	io:format("Best Asteroid for monitoring: ~p~n", [R]),
	{Ast, _} = R,
	R1 = sort_by_polar_angl(Ast, lists:delete(Ast, Asteroids)),
%%	io:format("~n***** R1 *****~n~p~n", [R1]),
%%	{N, V} = vaporizing(Ast, R1, R1, [], 1),
	vaporizing_loop(Ast, R1, 1).
%%	io:format("~n***** End *****~n~p ~p~n", [N, V]).

read_file(Name) ->
	{ok, F} = file:open(Name, read),
	lists:flatten(loop(F, 0, [])).

%% ====================================================================
%% Internal functions
%% ====================================================================

loop(File, Y, List) ->
	case io:get_line(File, '') of
		eof -> List;
		{error, _Descr} -> error;
		Line ->
			Coord = line_process(0, Y, Line),
			loop(File, Y + 1, [Coord | List])
	end.

line_process(_, _, []) -> [];
line_process(X, Y, [$# | Line]) ->
	[{X, Y} | line_process(X + 1, Y, Line)];
line_process(X, Y, [$. | Line]) ->
	line_process(X + 1, Y, Line);
line_process(_, _, [$\n | _]) -> [].

check_for_visibility(_A, [], _L, Counter) ->
	Counter;
check_for_visibility(A, [Am | As], L, Counter) when A == Am ->
	check_for_visibility(A, As, L, Counter);
check_for_visibility(A, [Am | As], L, Counter) ->
	IsHidden = is_hidden(A, L, Am),
%%	io:format(">>> A0=~p, Am=~p, hidden? = ~p~n", [A, Am, IsHidden]),
	if IsHidden -> check_for_visibility(A, As, L, Counter);
		 true -> check_for_visibility(A, As, L, Counter + 1)
	end.

is_hidden(_, [], _) -> false;
is_hidden(A0, [An | L], A1) when (A0 == An) or (A1 == An) ->
	is_hidden(A0, L, A1);
is_hidden({X0, Y0} = A0, [{Xn, Yn} = _An | L], {X1, Y1} = A1) ->
	DXn = Xn - X0,
	DYn = Yn - Y0,
	DX1 = X1 - X0,
	DY1 = Y1 - Y0,
	Is_Same_Site = (sign(DYn) == sign(DY1)) and (sign(DXn) == sign(DX1)),
	Is_On_Line = (DYn * DX1) == (DY1 * DXn),
	Is_Between = (abs(DX1) >= abs(DXn)) and (abs(DY1) >= abs(DYn)),
%%	io:format("    >>> A0=~p, A1=~p, An = ~p, IsSame = ~p, DXn = ~p, DYn = ~p, DX1 = ~p, DY1 = ~p~n", [A0, A1, _An, Is_Same_Site, DXn, DYn, DX1, DY1]),
	if Is_Same_Site and Is_On_Line and Is_Between -> true;
		 true -> is_hidden(A0, L, A1)
	end.

sign(N) when N >= 0 -> 1;
sign(_) -> -1.

fi({X, Y}) ->
	R = math:sqrt(X * X + Y * Y), 
	Arcos = math:acos(X / R),
	Fi =
	if Y >= 0 -> Arcos;
		 true -> - Arcos
	end - math:pi() / 2,
	if Fi =< 0 -> Fi + 2 * math:pi();
		 true -> Fi
	end.
	
sort_by_polar_angl({Xs, Ys}, L) ->
	Res = lists:map(fun({X, Y} = E) -> {E, fi({X - Xs, Ys - Y})} end, L),
%	io:format("~n***** Res *****~n~p~n", [Res]),
	Res1 = lists:sort(fun({_, Fi1}, {_, Fi2}) -> Fi1 >= Fi2 end, Res),
%	io:format("~n***** Res1 *****~n~p~n", [Res1]),
	lists:map(fun({E, _}) -> E end, Res1).

vaporizing(_MonAst, [], _Lorig, Vap, N) -> {N, Vap};
vaporizing(MonAst, [As | L], Lorig, Vap, N) ->
	IsHidden = is_hidden(MonAst, Lorig, As),
	if IsHidden -> vaporizing(MonAst, L, Lorig, Vap, N);
		 true -> 
			if (N > 195) and (N < 205) -> io:format("**** ~p-th vaporized Asteroid: ~p~n", [N, As]);
				 true -> skip
			end,
			vaporizing(MonAst, L, Lorig, [As | Vap], N + 1)
	end.

vaporizing_loop(_MonAst, [], N) -> N;
vaporizing_loop(MonAst, Lo, N) ->
	{NN, V} = vaporizing(MonAst, Lo, Lo, [], N),
	LoNew = Lo -- V,
	io:format("=== vaporizing ~p asteroid. ~p ~p ~p~n", [NN, length(Lo), length(V), length(LoNew)]),
	vaporizing_loop(MonAst, LoNew, NN).


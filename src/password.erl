%% @author alexei
%% @doc @todo Add description to password.


-module(password).

%% ====================================================================
%% API functions
%% ====================================================================
-export([has_doubles/1, has_only_doubles/2, is_increase/1, next/3, loop/3, run/0, run1/0]).

run() ->
	loop([5,4,3,2,8,3], [7,6,1,3,4,8], 0).

loop(R, End, Acc) ->
	Cond = is_increase(R) and has_doubles(R),
	New_acc =
	if Cond -> 
			 io:format("> ~p~n", [R]),
			 Acc + 1;
		 true -> Acc
	end,
	if R == End -> New_acc;
		 true -> loop(next(R, 1, []), End, New_acc)
	end.

run1() ->
	loop1([5,4,3,2,8,3], [7,6,1,3,4,8], 0).

loop1(R, End, Acc) ->
	Cond = is_increase(R) and has_only_doubles(R, 0),
	New_acc =
	if Cond -> 
			 io:format("> ~p~n", [R]),
			 Acc + 1;
		 true -> Acc
	end,
	if R == End -> New_acc;
		 true -> loop1(next(R, 1, []), End, New_acc)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
next([], _, R) -> lists:reverse(R);
next([A | L], P, R) ->
	B = A + P,
	if B > 9 -> next(L, 1, [0 | R]);
		 true -> next(L, 0, [B | R])
	end.

is_increase([A, B | []]) when A >= B -> true;
is_increase([_, _ | []]) -> false;
is_increase([A, B | L]) when A >= B ->
	is_increase([B | L]);
is_increase(_) -> false.

has_doubles([A, B | []]) when A == B -> true;
has_doubles([_, _ | []]) -> false;
has_doubles([A, B | _]) when A == B -> true;
has_doubles([_, B | L]) ->
	has_doubles([ B | L]).

has_only_doubles([], 1) -> true;
has_only_doubles([], _) -> false;
has_only_doubles([_], 1) -> true;
has_only_doubles([_], _) -> false;
has_only_doubles([A, B | L], Counter) when A == B ->
	has_only_doubles([B | L], Counter + 1);
has_only_doubles([A, B | _], 1) when A /= B -> true;
has_only_doubles([A, B | L], _) when A /= B ->
	has_only_doubles([B | L], 0).

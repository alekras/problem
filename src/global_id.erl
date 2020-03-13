%% @author alexei
%% @doc @todo Add description to global_id.

-module(global_id).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, run_guid/1, get_id/0, test/0]).


get_id() -> 
	global_id_srv ! {guid, self()},
	receive
		Guid -> Guid
	end.

% start process that is generated unique id.
start() -> 
	Pid = spawn(global_id, run_guid, [0]),
	register(global_id_srv, Pid).

% stop process.
stop() ->
	global_id_srv ! stop,
	unregister(global_id_srv).

% process loop function.
% GUID is formed as
% 42 bits timestamp (now it is ~ 1584120792985 < 2 ^ 41 so 42 bits will be enough)
% 3 bits space for future use.
% 8 bits counter of a request during 1 millisecond. It has to be < 100(< 2^7), so 2^8 will be enough.
% 11 bits for node id that is <= 1024 (2^10).
run_guid(Count) ->
	receive
		{guid, Pid} -> 
			<<Int:64/integer>> = <<(timestamp()):42, 0:3, Count:8, (node_id()):11>>,
			Pid ! Int,
			run_guid((Count + 1) rem 256);
		stop -> stop
	end.

%% test global id
test() ->
	start(),
%% Test format and value generated GUID.
	Loop1 = fun 
		Loop(0) -> stop;
		Loop(N) -> 
			Guid = get_id(),
			<<Ts:42, Space:3, Count:8, Node_id:11>> = <<Guid:64/integer>>,
			io:format("GUID  = ~p   <<~p,~p,~p,~p>>~n", [Guid, Ts, Space, Count, Node_id]),
			Loop(N -1)
		end,
	Loop1(20),

% generate 400 ids on maximum speed.
	Loop2 = fun 
		Loop(0, RL) -> RL;
		Loop(N, RL) ->
			Loop(N -1, [get_id() | RL])
		end,
	Result = Loop2(400, []), %% List of 400 ids
	Result1 = [ begin <<Ts:42, Space:3, Count:8, Node_id:11>> = <<G:64/integer>>, 
%%										io:format("G:~p, TS:~p ~p ~p ~p~n", [G, Ts, Space, Count, Node_id]), 
										Ts 
							end || G <- Result], %% list of 400 timestamps
	
%% Test how much ids are generated for 1 millisecond.
%% function returns list of number of ids generated during each millisecond
	Count = fun
		Counter([], C) -> C;
		Counter([Ts1, Ts2], [E | CC] = C) ->
			if Ts1 == Ts2 -> Counter([], [E + 1 | CC]);
				 true -> Counter([], [1 | C])
			end;
		Counter([Ts1, Ts2 | R], [E]) ->
			if Ts1 == Ts2 -> Counter([Ts2 | R], [E + 1]);
				 true -> Counter([Ts2 | R], [1 , E])
			end;
		Counter([Ts1, Ts2 | R], [E | CC] = C) ->
			if Ts1 == Ts2 -> Counter([Ts2 | R], [E + 1 | CC]);
				 true -> Counter([Ts2 | R], [1 | C])
			end
	end,
	io:format("Counts for each millisecond = ~w~n", [Count(Result1, [1])]),

%% Check if all generated ids are unique. If we have more then 256 id per millisecond then ids will be not unique.
	Check_uniqueness = fun
		Check([], C) -> C;
		Check([G | LGs], C) ->
			Unique = lists:any(fun(E) -> G == E end, LGs),
			if Unique -> 
					 <<Ts1:42, Space1:3, Count1:8, Node_id1:11>> = <<G:64/integer>>,
					 io:format("Non unique = ~w <<~p, ~p, ~p, ~p>> ~n", [G, Ts1, Space1, Count1, Node_id1]),
					 Check(LGs, C + 1);
				 true -> Check(LGs, C)
			end
		end,
	io:format("Is all unique? (0 - yes) = ~p~n", [Check_uniqueness(Result, 0)]),
	
	stop().
%% ====================================================================
%% Internal functions
%% ====================================================================

%% Returns your node id as an integer.
%%   It will be greater than or equal to 0 and less than or equal to 1024.
%%   It is guaranteed to be globally unique.
node_id() ->
	1024.

%% Returns timestamp since the epoch in milliseconds.
timestamp() ->
	os:system_time(millisecond).

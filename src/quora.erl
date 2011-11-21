%% Author: eschneef
%% Created: Nov 14, 2011
%% Description: quora challenge
-module(quora).

-define(TIMEOUT, 5000).

%%
%% Exported Functions
%%
-export([start/1, go/0, find_neighbors/5, get_key/2]).

%%
%% API Functions
%%
go() ->
	start(["d3.txt"]).

start([Filename]) ->
	case file:open(Filename, [read]) of
		{ok, Fid} ->
			ets:new(rooms, [named_table, public]),
			{W, H} = read_header(Fid),
			ets:insert(rooms, {w, W}),
			ets:insert(rooms, {h, H}),
			ets:insert(rooms, {count, W * H }),
			Bin = read_rooms(Fid, 1, <<>>),
			run_ducts(Bin),
			ets:delete(rooms);
		{error, Error} ->
			{error, Error}
	end.


%%
%% Local Functions
%%
read_header(Fid) ->
	[Wstr, Hstr] = string:tokens(io:get_line(Fid, []), " \t\n"),
	{list_to_integer(Wstr), list_to_integer(Hstr)}.


read_rooms(Fid, H, Bin) ->
	case io:get_line(Fid, []) of
		eof ->
			io:fwrite("Bin: ~p~n", [Bin]),
			Bin;
		Line ->
			Rooms = string:tokens(Line, " \t\n"),
			read_rooms(Fid, H + 1, create_room(1, H, Rooms, Bin))
	end.

create_room(_, _, [], Bin) ->
	Bin;
create_room(W, H, [Head|Tail], Bin) ->
	Key = list_to_integer(Head),
	Count = ets:lookup_element(rooms, count, 2),
	case Key of
		2 -> ets:insert(rooms, {start, {W, H}});
		1 -> ets:insert(rooms, {count, Count - 1});
		_ -> skip
	end,
	build_map(W, H),
	create_room(W + 1, H, Tail, <<Bin/binary, Key:4, 0:12>>).

build_map(W, H) ->
	MaxW = ets:lookup_element(rooms, w, 2),
	MaxH = ets:lookup_element(rooms, h, 2),
	List = [{NextW, NextH} ||
			NextW <- [W-1, W, W+1],
			NextH <- [H-1, H, H+1],
			{W, H} /= {NextW, NextH},
			((NextW == W) or (NextH == H)),
			NextW > 0, NextW =< MaxW,
			NextH > 0, NextH =< MaxH
		   ],
	%% io:fwrite("&& ~w ~w && ~p~n", [W, H, List]),
	ets:insert(rooms, {{map, W, H}, 
					   lists:map(
						 fun({X, Y}) -> {X, Y, (X - 1) + (Y - 1) * MaxW} end,
						 List)}).

run_ducts(Bin) ->
	%% find the start
	[[StartW, StartH]] = ets:match(rooms, { start, {'$1', '$2'}}),
	[{_, MaxW}] = ets:lookup(rooms, w),
	%% io:fwrite("starting at ~B ~B : ~B ~B~n", [StartW, StartH, MaxW, MaxH]),
	register(collector, spawn(fun() -> collect(0, [], 0) end)),
	register(top, self()),
	spawn(fun() -> find_neighbors([{StartW, StartH}], MaxW, 1, Bin, push) end),
	receive
		stop ->
			io:fwrite("clean stop~n", []),
			done
		after (?TIMEOUT * 2) ->
			io:fwrite("second timeout~n", []),
			done
	end.
%% init:stop().

find_neighbors(From = [{W, H}|_Tail], MaxW, N, Bin, Action) ->
	%% io:fwrite("level ~B <~B ~B>~n", [N, W, H]),
	case Action of
		push -> collector ! {start, top, N};
		_ -> skip
	end,
	{NewBin, Key} = get_offsets(W, H, MaxW, Bin),
	%% io:fwrite("Key ~w~n", [Key]),
	case Key of
		3 ->
			case ets:lookup_element(rooms, count, 2) of
				Count when Count == length(From) ->
					io:fwrite("found end ~B ~B ~B ~p~n", [N, length(From), Count, From]),
					collector ! {good, top, N + 1};
				Count ->
					collector ! {short, top, N + 1}
			end;	
		_ ->
			List = get_next_rooms(W, H, Bin),
			%% io:fwrite("List is ~p~n", [List]),
			case List of
				[] ->
					collector ! {bad, top, N};
				[{W1, H1}] ->
					find_neighbors([{W1, H1}|From], MaxW, N + 1, NewBin, skip);
				_ ->
					spinoff(List, From, MaxW, NewBin, N + 1),
					collector ! {done, top, N}
			end				
	end.

get_next_rooms(W, H, Bin) ->
	io:fwrite("** ~w ~w <<~w>>~n", [W, H, ets:lookup(rooms, {map, W, H})]),
	case ets:lookup(rooms, {map, W, H}) of
		[{{map, W, H}, List}] ->
			[{X, Y} || {X, Y, Z} <- List, check_cell(Z, Bin)];
		_ ->
			io:fwrite("Bad lookup ~w ~w~n", [W, H]),
			[]
	end.

check_cell(Offset, Bin) ->
	<<_:Offset/unit:16, Key:4, _:12, _/bitstring>> = Bin,
	case Key of
		1 -> false;
		3 -> true;
		0 -> true
	end.

spinoff([], _, _, _, _) ->
	done;
spinoff([H|T], From, MaxW, Bin, N) ->
	spawn(fun() -> find_neighbors([H|From], MaxW, N + 1, Bin, push) end),
	spinoff(T, From, MaxW, Bin, N + 1).

get_key(X, Y) ->
	%% io:fwrite("in get_key at ~w ~w~n", [X, Y]),
	case ets:lookup(rooms, {room, X, Y}) of
		[{_, Key}] -> Key;
		_ ->
			io:fwrite("key lookup failed at ~B ~B~n", [X, Y]),
			-1
	end.

get_offsets(W, H, MaxW, Bin) ->
	%% io:fwrite("in get_offsets~n", []),
	case ((W - 1) + (H - 1) * MaxW) of
		0 ->
			<<Key:4, Order:12, Tail/binary>> = Bin,
			{<<1:4, (Order + 1):12, Tail/binary>>, Key};
		Offset ->
			<<Head:Offset/unit:16, Key:4, Order:12, Tail/binary>> = Bin,
			{<<Head:Offset/unit:16, 1:4, (Order + 1):12, Tail/binary>>, Key}
	end.

collect(0, _, Found) when Found > 0 ->
	io:fwrite("~w good runs~n", [Found]),
	top ! stop;
collect(N, Ps, Found) ->
	receive
		{start, From, _Depth} ->
			io:fwrite("new process ~w [~w] ~w~n", [N + 1, From, _Depth]),
			collect(N + 1, [From|Ps], Found);
		{done, From, _Depth} ->
			io:fwrite("just spawned more ~w [~w] ~w~n", [N, From, _Depth]),
			collect(N - 1, Ps, Found);
		{bad, From, _Depth} ->
			io:fwrite("ending process ~w [~w] ~w~n", [N, From, _Depth]),
			collect(N - 1, Ps -- [From], Found);
		{short, From, _Depth} ->
			io:fwrite("ending process ~w [~w] ~w~n", [N, From, _Depth]),
			collect(N - 1, Ps -- [From], Found);
		{good, From, Depth} ->
			io:fwrite("good run ~w [~w] ~w~n", [N, From, Depth]),
			collect(N - 1, Ps, Found + 1);
		{unknown, From, Depth} ->
			io:fwrite("unknown ~w [~w] ~w~n", [N, From, Depth]),
			collect(N, Ps, Found)
		after ?TIMEOUT ->
			io:fwrite("timed out ~w~n", [N]),
			top ! stop
	end.

%% @author pradeeppeiris
%% @doc @todo Add description to logger.


-module(logger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

%% ====================================================================
%% Internal functions
%% ====================================================================

init(Nodes) ->
	Clock = api:clock(Nodes),
    loop(Clock, []).

loop(Clock, HoldbackQueue) ->
	receive
		{log, From, Time, Msg} ->
%%			Update the Clock 			
			UpdatedClock = api:update(From, Time, Clock),
			
%%			Add message to hold-back queue 			
			NewHoldbackQueue = lists:append([{From, Time, Msg}], HoldbackQueue),
			
%%			Log messages
%% 			io:format("<<<<<: ~w ~n", [UpdatedClock]),
			UpdatedHoldbackQueue = update(NewHoldbackQueue, UpdatedClock), 
%%  		io:format(">>>>>: ~w ~n", [UpdatedHoldbackQueue]),
%% 			io:format(">>>>>: ~n", []),
			
%% 			log(From, Time, Msg),
%% 			io:format("test: ~w ~n", [UpdatedClock]),
			loop(UpdatedClock, UpdatedHoldbackQueue);
		stop ->
			ok
	end.


log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
	
update(HoldbackQueue, Clock) ->
	lists:foldl(fun({From, Time, Msg}, UpdatedHoldbackQueue) -> 
						  	case (api:safe(Time, Clock)) of
								true -> 
									log(From, Time, Msg),
									UpdatedHoldbackQueue;
								false ->
									UpdatedHoldbackQueueForSort = lists:append([{From, Time, Msg}], UpdatedHoldbackQueue),
									lists:sort(fun({_, X, _}, {_, Y, _}) -> Y < X end, UpdatedHoldbackQueueForSort)
							end
						  end, [], HoldbackQueue).
	

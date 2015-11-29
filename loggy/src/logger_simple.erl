%% @author pradeeppeiris
%% @doc @todo Add description to logger_simple.


-module(logger_simple).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
   Logger ! stop.

init(_) ->
    loop().

%% ====================================================================
%% Internal functions
%% ====================================================================

loop() ->
    receive
        {log, From, Time, Msg} ->
			log(From, Time, Msg),
            loop();
        stop ->
			ok 
	end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).



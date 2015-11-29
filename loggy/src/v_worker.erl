%% @author pradeeppeiris
%% @doc @todo Add description to v_worker.


-module(v_worker).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
	random:seed(Seed, Seed, Seed),
	LamportTime = v_time:zero(),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, LamportTime);
		stop ->
			ok
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================



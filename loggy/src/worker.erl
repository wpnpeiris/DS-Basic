%% @author pradeeppeiris
%% @doc @todo Add description to worker.


-module(worker).

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
	LamportTime = api:zero(),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, LamportTime);
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.



%% ====================================================================
%% Internal functions
%% ====================================================================
loop(Name, Log, Peers, Sleep, Jitter, LamportTime)->
	Wait = random:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			ReceiveTime = api:inc(Name, api:merge(Time, LamportTime)),
			Log ! {log, Name, ReceiveTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, ReceiveTime);
		stop -> ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
	
	after Wait ->
		Selected = select(Peers),
        Time = api:inc(Name, LamportTime),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
		jitter(Jitter),
		Log ! {log, Name, Time, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).


	
	


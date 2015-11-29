%% @author pradeeppeiris
%% @doc @todo Add description to gms2_1.


-module(gms2_1).

-define(timeout, 20).
-define(arghh, 50).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start/2]).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> initleader(Id, Rnd, Self) end)}.

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun()-> initslave(Id, Grp, Self) end)}.



%% ====================================================================
%% Internal functions
%% ====================================================================
initleader(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]).

initslave(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
            Master ! {view, Group},	
            slave(Id, Master, Leader, Slaves, Group)

		after ?timeout ->
    		Master ! {error, "No reply from Leader"}
	end.

leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
stop -> ok
end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
		{'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);
        stop ->
	ok
end.

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
	end.

bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> 
					case random:uniform(?arghh) of
						?arghh ->
							io:format("missing message~n", []);
						_ ->
							Node ! Msg
					end
				  end, 
				Nodes).


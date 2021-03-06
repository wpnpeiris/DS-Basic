%% @author pradeeppeiris
%% @doc @todo Add description to gms3.


-module(gms3).

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
    leader(Id, Master, 0, [], [Master]).

initslave(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
            Master ! {view, Group},	
            slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group)

		after ?timeout ->
    		Master ! {error, "No reply from Leader"}
	end.

leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
stop -> ok
end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);

        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);

		{msg, I, _} when I < N ->
      		slave(Id, Master, Leader, N, Last, Slaves, Group);
		
		{msg, I, Message} ->
			Master ! Message,
      		slave(Id, Master, Leader, I+1, {msg, I, Message}, Slaves, Group);

        {view, I, [Leader|UpdatedSlaves], UpdatedGroup} ->
			Master ! {view, UpdatedGroup},
      		slave(Id, Master, Leader, I, {view, I, [Leader|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves, UpdatedGroup);
       
		{'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
			ok
end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
        	bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
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
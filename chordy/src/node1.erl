%% @author pradeeppeiris
%% @doc @todo Add description to node1.


-module(node1).

-define(Stabilize, 10000).
-define(Timeout, 10000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start/2, stabilize/1]).

start(Id) ->
    start(Id, {nil, nilpid}).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).


%% ====================================================================
%% Internal functions
%% ====================================================================
init(Id, Peer) ->
    Predecessor = {nil, nilpid},
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, {nil, nilpid}) ->
	{ok, {Id, self()}};

connect(_, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor) ->
	{PreID, _} = Predecessor,
	{SucID, _} = Successor,
	io:format("<< STATUS: ~w , Successor: ~w, Predecessor: ~w >> ~n", [Id, SucID, PreID]),
    receive
		probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
		
		{probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
		
		{probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
		
        {key, Qref, Peer} ->
			io:format("[~w]: A peer needs to know our key ~n", [Id]),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
		
        {notify, New} ->
			io:format("[~w]: A new node informs us of its existence ~n", [Id]),
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
		
        {request, Peer} ->
			io:format("[~w]: A predecessor needs to know our predecessor ~n", [Id]),
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
		
        {status, Pred} ->
			io:format("[~w]: Our successor informs us about its predecessor ~n", [Id]),
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
		
		stabilize ->
			io:format("[~w]: Call for stablize ~n", [Id]),
            stabilize(Successor),
            node(Id, Predecessor, Successor)
end.

create_probe(Id, {_, Spid}) ->
 	 T = erlang:now(),
 	 io:format(">>> Start Probe from [~w] at ~w ~n", [Id, T]),
  	Spid ! {probe, Id, [Id], T}.

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
	io:format(">>> forward Probe from [~w] ~n", [Id]),
  	Spid ! {probe, Ref,  [Nodes | Id], T}.

remove_probe(T, _) ->
  	TimeDiff = timer:now_diff(erlang:now(), T),
  	io:format(">>> Completed Probe in ~w ~n", [TimeDiff]).

stabilize({_, Spid}) ->
	Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        {nil, nilpid} ->
            Peer ! {status, {nil, nilpid}};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
	end.

notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		{nil, nilpid} ->
			{Nkey, Npid};
		{Pkey,  _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					{Nkey, Npid};
				false ->
					Predecessor
			end
	end.
			 
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		{nil, nilpid} ->
			io:format("(~w) No Predecessor, inform Successor (~w) about existance ~n", [Id, Skey]),
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} ->
			io:format("(~w) Predecessor is myself, nothing to do ~n", [Id]),
			Successor;
		{Skey, _} ->
			io:format("(~w) No Predecessor but only successor, inform Successor (~w) existance ~n", [Id, Skey]),
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					io:format("(~w) Predecessor needs to know predecessor of (~w) ~n", [Id, Skey]),
					 Xpid ! {request, self()},
          			{Xkey, Xpid};
				false ->
					io:format("(~w) No Predecessor, inform Successor (~w) about existance ~n", [Id, Skey]),
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).
	
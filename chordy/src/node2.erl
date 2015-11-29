%% @author pradeeppeiris
%% @doc @todo Add description to node2.


-module(node2).

-define(Stabilize, 10000).
-define(Timeout, 10000).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start/2, stabilize/1]).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).


%% ====================================================================
%% Internal functions
%% ====================================================================
init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
	Store = storage:create(),
    node(Id, Predecessor, Successor, Store).

connect(Id, nil) ->
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

node(Id, Predecessor, Successor, Store) ->
	%%io:format("<< STATUS: ~w , Successor: ~w, Predecessor: ~w, Store Size: ~w >> ~n", [Id, Successor, Predecessor, erlang:length(Store)]),
	io:format("<< STATUS: ~w , Store Size: ~w >> ~n", [Id, erlang:length(Store)]),
    receive
		probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
		
		{probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
		
		{probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
		
        {key, Qref, Peer} ->
%% 			io:format("[~w]: A peer needs to know our key ~n", [Id]),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
		
        {notify, New} ->
%% 			io:format("[~w]: A new node informs us of its existence ~n", [Id]),
            {Pred, UpdateStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, UpdateStore);
		
        {request, Peer} ->
%% 			io:format("[~w]: A predecessor needs to know our predecessor ~n", [Id]),
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
		
        {status, Pred} ->
%% 			io:format("[~w]: Our successor informs us about its predecessor ~n", [Id]),
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
		
		stabilize ->
%% 			io:format("[~w]: Call for stablize ~n", [Id]),
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
		
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added);
		
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store);
		
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged)
end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Client ! {Qref, storage:lookup(Key, Store)};
		false ->
			Spid ! {lookup, Key, Qref, Client}
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
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
	end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			{{Nkey, Npid}, Store};
		{Pkey,  _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Id, Store, Nkey, Npid),
					{{Nkey, Npid}, Keep};
				false ->
					{Predecessor, Store}
			end
	end.

handover(Id, Store, Nkey, Npid) ->
	{Keep, Rest} = storage:split(Id, Nkey, Store),
	Npid ! {handover, Rest},
	Keep.
	
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
 			%%io:format("(~w) No Predecessor, inform Successor (~w) about existance ~n", [Id, Skey]),
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} ->
 			%%io:format("(~w) Predecessor is myself, nothing to do ~n", [Id]),
			Successor;
		{Skey, _} ->
 			%%io:format("(~w) No Predecessor but only successor, inform Successor (~w) existance ~n", [Id, Skey]),
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
 					%%io:format("(~w) A new Predecessor inbetween (~w) ~n", [Id, Skey]),
					 Xpid ! {request, self()},
          			{Xkey, Xpid};
				false ->
 					%%io:format("(~w)  A new Predecessor, outside (~w) about existance ~n", [Id, Skey]),
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).
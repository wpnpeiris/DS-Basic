%% @author pradeeppeiris
%% @doc @todo Add description to routy1.


-module(routy1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
	Node ! stop,
	unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = [hist:new(Name)],
    router(Name, 0, Hist, Intf, Table, Map).

%% ====================================================================
%% Internal functions
%% ====================================================================
router(Name, N, Hist, Intf, Table, Map) ->
	receive

		{links, Node, R, Links} ->
			io:format("~w: Recived Link-State from ~w~n", [Name, Node]),
			case hist:update(Node, R, Hist) of
				{new, Hist1} ->
					intf:broadcast({links, Node, R, Links}, Intf),
					Map1 = map:update(Node, Links, Map),
					io:format("	~w: Updated Map ~w~n", [Name, Map1]),

					Table1 = dijkstra:table(intf:list(Intf), Map1),
					io:format("	~w: Updated Routing Table ~w~n", [Name, Table1]),

                  	router(Name, N, Hist1, Intf, Table1, Map1);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;	

		{add, Node, Pid} ->
			io:format("~w: Recived Add from ~w~n", [Name, Node]),
			Ref = erlang:monitor(process,Pid),	
			Intf1 = intf:add(Node, Ref, Pid, Intf),
			router(Name, N, Hist, Intf1, Table, Map);

		{remove, Node} ->
			io:format("~w: Recived Remove from ~w~n", [Name, Node]),
			{ok, Ref} = intf:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = intf:remove(Node, Intf),
			router(Name, N, Hist, Intf1, Table, Map);

		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = intf:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

		{status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

		{route, Name, From, Message} ->
			io:format("~w: received message ~w ~n", [Name, Message]),
			router(Name, N, Hist, Intf, Table, Map);
		
		{route, To, From, Message} ->
			io:format("~w: routing message (~w)", [Name, Message]),
			case dijkstra:route(To, Table) of
				{ok, Gw} ->
					case intf:lookup(Gw, Intf) of
						{ok, Pid} ->
							Pid ! {route, To, From, Message};
						notfound ->
							ok
					end;
				notfound ->
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);
		
		{send, To, Message} ->
        	self() ! {route, To, Name, Message},
        	router(Name, N, Hist, Intf, Table, Map);

		update ->
			io:format("Recived Update To message ~w~n", [Name]),
        	Table1 = dijkstra:table(intf:list(Intf), Map),
          	router(Name, N, Hist, Intf, Table1, Map);

		{broadcast, To} ->
			io:format("Recived Broadcast To message ~w~n", [Name]),
         	Message = {links, Name, N, intf:list(Intf)},
			To ! Message,
          	router(Name, N+1, Hist, Intf, Table, Map);

		info ->
			io:format("List Interfaces: ~n", []),
			lists:map(fun(X) -> io:format(" >>> ~s~n", [X]) end , intf:list(Intf)),
			
			io:format("Network Map: ~n", []),
			io:format("	>>> ~w~n", [Map]),

			io:format("Routing Table: ~n", []),
			io:format("	>>> ~w~n", [Table]),
			router(Name, N, Hist, Intf, Table, Map);

		stop ->
			ok
	end.	

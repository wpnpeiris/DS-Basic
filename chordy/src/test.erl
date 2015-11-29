%% @author pradeeppeiris
%% @doc @todo Add description to test.


-module(test).
-compile(export_all).

-define(Timeout, 1000).

%% ====================================================================
%% API functions
%% ====================================================================


start(Module) ->
    Id = key:generate(), 
    apply(Module, start, [Id]).


start(Module, P) ->
    Id = key:generate(), 
    apply(Module, start, [Id,P]).    

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N-1, P).


%% Setup ring with N nodes.
setup(N, Peer) ->
	case N of
		0 -> 
			Peer;
		X ->
			Node = start(node2, Peer),
			timer:sleep(500),
			setup(X-1, Node)
	end.
			
		
	

%% The functions add and lookup can be used to test if a DHT works.

add(Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive 
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    Node ! {lookup, Key, Q, self()},
    receive
     {Q, Value} ->
         {Key, Value}
        after ?Timeout ->
         {error, "timeout"}
    end.

%lookup(Key, Node) ->
%    Q = make_ref(),
%    Node ! {lookup, Key, Q, self()},
%    receive 
%	{Q, Value} ->
%	    Value
%    after ?Timeout ->
%	    {error, "timeout"}
%    end.

%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, gurka, P) end, Keys).

check(Keys, P) ->
    T1 = erlang:monotonic_time(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = erlang:monotonic_time(),
    Done = ((T2 - T1) div 1000),
    io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
    io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).


check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key,P) of
    	{Key, _} ->
    	    check(Keys, P, Failed, Timeout);
    	{error, _} ->
    	    check(Keys, P, Failed, Timeout+1);
    	false ->
    	    check(Keys, P, Failed+1, Timeout)
    end.


addTestStore(P) ->
	add(100, a , P),
	add(200, b , P),
	add(300, c , P),
	add(400, d , P),
	add(500, e , P),
	add(600, f , P),
	add(700, g , P),
	add(800, h , P),
	add(900, i , P).
	
%% ====================================================================
%% Internal functions
%% ====================================================================



%% @author pradeeppeiris
%% @doc @todo Add description to time.


-module(time).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2] ).

zero() -> 0.

inc(_, T) -> T + 1.

merge(Ti, Tj) -> 
	if 
		Ti > Tj -> Ti;
		Ti < Tj -> Tj;
		Ti == Tj -> Ti
	end.
	
leq(Ti,Tj)  -> Ti =< Tj.
	
clock(Nodes) ->
	lists:foldl(fun(Node, ClockNodes) -> lists:append([{Node, 0}], ClockNodes) end, [], Nodes).

update(Node, Time, Clock) ->
	lists:append([{Node, Time}], lists:keydelete(Node, 1, Clock)).
	
safe(Time, Clock) ->
	lists:foldl(fun({_, T}, Flag) -> leq(Time, T) and Flag end, true, Clock).

%% ====================================================================
%% Internal functions
%% ====================================================================



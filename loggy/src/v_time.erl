%% @author pradeeppeiris
%% @doc @todo Add description to v_time.


-module(v_time).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zero/0, new/1, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> [].

inc(Node, T) -> 
	case lists:keyfind(Node, 1, T) of
		{Node, Time} -> lists:keyreplace(Node, 1, T, {Node, Time + 1});
		false -> lists:append([{Node, 1}], T)
	end.

new(Nodes) ->
	lists:foldl(fun(Node, VectorClocks) -> lists:append([{Node, 0}], VectorClocks) end, [], Nodes).

%% inc(Node, Clock) -> 
%% 	{Node, T} = lists:keyfind(Node, 1, Clock),
%% 	ClockUpdated = lists:keydelete(Node, 1, Clock),
%% 	lists:append([{Node, T + 1}], ClockUpdated).

merge([], T) -> T;
merge(Ci, Cj) ->
	lists:foldl(fun({Node, Ti}, C) ->  
					case lists:keyfind(Node, 1, Cj) of 
						{Node, Tj} ->
							if
								Ti > Tj -> lists:append([{Node, Ti}], C); % [{Node, Ti} | C]
								Ti < Tj -> lists:append([{Node, Tj}], C);
								Ti == Tj -> lists:append([{Node, Ti}], C)
							end;
						false -> lists:append([{Node, Ti}], C)
					end
				end, [], Ci).

leq([], _) -> true;
	
leq([{Name, Ti} | Rest], Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			if
				Ti =< Tj ->
					leq(Rest, Time);
				true ->
					false
			end;
		false ->
			false
	end.

clock(Nodes) ->
	lists:foldl(fun(Node, ClockNodes) -> lists:append([{Node, 0}], ClockNodes) end, [], Nodes).

update(Node, Time, Clock) ->
	case lists:keyfind(Node, 1, Clock) of
		{Node, _} ->
			lists:keyreplace(Node, 1, Clock, {Node, Time});
		false ->
			[{Node, Time}| Clock]
	end.

safe(Time, Clock) -> leq(Time, Clock).
	
	
%% ====================================================================
%% Internal functions
%% ====================================================================



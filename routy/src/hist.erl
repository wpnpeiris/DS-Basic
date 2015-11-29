%% @author pradeeppeiris
%% @doc @todo Add description to hist.


-module(hist).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, update/3]).

new(Name) -> {Name, 0}.

update(Node, N, History) ->
	case lists:keyfind(Node, 1, History) of
		{Node, H} -> 
			io:format("History Exits for Node: ~s~n", [Node]),		
			case N =< H of %% consider =, otherwise equal will be consider as new
				true ->
					io:format("	Old Message with Flag, and ignore: ~w~n", [N]),
					old;
				
				false ->
					io:format("	New Message with Flag: ~w~n", [N]),
					{new, lists:append([{Node, N}], lists:keydelete(Node, 1, History))}
			end;

		false -> 
			io:format("	First Message from Node: ~s~n", [Node]),
			{new , lists:append([new(Node)], History)}
	end.
			
	

%% ====================================================================
%% Internal functions
%% ====================================================================



%% @author pradeeppeiris
%% @doc @todo Add description to map.


-module(map).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() -> [].

update(Node, Links, Map) ->
	MapUpdated = lists:keydelete(Node, 1, Map),
	lists:append([{Node, Links}], MapUpdated).
	

reachable(Node, Map) ->
	lists:me

all_nodes(Map) ->
	lists:foldl(fun({PNode, Links}, AllNodes) ->
					lists:foldl(fun(Node, AllNodes_) -> add(Node, AllNodes_) end, add(PNode, AllNodes), Links)
				end, 
				[], Map).


	%%lists:foldl(fun({Node, Links}, AllNodes) -> lists:append([Node | Links], AllNodes) end, [], Map).

	
%% ====================================================================
%% Internal functions
%% ====================================================================
add(Node, List) ->
	case lists:member(Node, List) of
		true -> List;
		false -> lists:append([Node], List)
	end.


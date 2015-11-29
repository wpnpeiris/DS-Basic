%% @author pradeeppeiris
%% @doc @todo Add description to dijkstra.


-module(dijkstra).

%% ====================================================================
%% API functions
%% ====================================================================
-export([table/2, route/2]).

table(Gateways, Map) ->
	Dummy = lists:foldl(fun(Node, Sorted) -> 
									lists:append([{Node, inf, unknown}], Sorted)
								end, 
								[], map:all_nodes(Map)),
	
	SortedInit = lists:foldl(fun(Node, Sorted) -> update(Node, 0, Node, Sorted) end, Dummy, Gateways),
	iterate(SortedInit, Map, []).

route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		{Node, Gateway} -> {ok, Gateway};
		false -> notfound
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		{_, N, _} -> N;
		false -> 0
	end.

replace(Node, N, Gateway, Sorted) ->
	SortedDeleted = lists:keydelete(Node, 1, Sorted),
	SortedUpdated = lists:append([{Node, N, Gateway}], SortedDeleted),
	lists:sort(fun({_, X, _}, {_, Y, _}) -> Y > X end, SortedUpdated).

update(Node, N, Gateway, Sorted) ->
	Entry = entry(Node, Sorted),
	case Entry > N of
		true -> replace(Node, N, Gateway, Sorted);
		false -> Sorted
	end.

iterate([], _, Table) -> Table;
iterate([{_, inf, unknown}|_], _, Table) -> Table;
iterate([{NodeTo, N, NodeFrom}|T], Map, Table) -> 
	ReachableNodes = map:reachable(NodeTo, Map),
	SortedUptd = lists:foldl(fun(Node, SortedList) -> update(Node, N + 1, NodeFrom, SortedList) end, T, ReachableNodes),
	UpdatedTable = lists:append([{NodeTo, NodeFrom}], Table),
	%%UpdatedTable = [{NodeTo, NodeFrom} | Table]
	
	iterate(SortedUptd, Map, UpdatedTable).


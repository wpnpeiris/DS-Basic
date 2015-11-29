%% @author pradeeppeiris
%% @doc @todo Add description to intf.


-module(intf).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() -> [].

add(Name, Ref, Pid, Intf) ->
	io:format("Add Interface: ~s~n", [Name]),
	lists:append([{Name, Ref, Pid}], Intf).

remove(Name, Intf) ->
	io:format("Remove Interface: ~s~n", [Name]),
	lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
	case lists:keyfind(Name, 1, Intf) of
		{Name, _, Pid} -> {ok, Pid};
		false -> notfound
	end.

ref(Name, Intf) ->
	case lists:keyfind(Name, 1, Intf) of
		{_, Ref, _} -> {ok, Ref};
		false -> notfound
	end.

name(Ref, Intf) -> 
	case lists:keyfind(Ref, 2, Intf) of
		{Name, _, _} -> {ok, Name};
		false -> notfound
	end.

list(Intf) ->
	lists:foldl(fun({Name, _, _}, NameList) -> lists:append([Name], NameList) end, [], Intf).

broadcast(Message, Intf) -> 
	lists:foreach(fun({_, _, Pid}) -> Pid ! Message end, Intf).
	


%% ====================================================================
%% Internal functions
%% ====================================================================



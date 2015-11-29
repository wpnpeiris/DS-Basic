%% @author pradeeppeiris
%% @doc @todo Add description to storage.


-module(storage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0, add/3, lookup/2, merge/2, split/3]).

create() -> [].

add(Key, Value, Store) ->
	lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key, Store) ->
	lists:keyfind(Key, 1, Store).

split(_, _, []) -> {[], []};
split(From, To, Store) ->
	lists:foldl(fun({Key, Value}, {Updated, Rest}) -> 
					case key:between(Key, From, To) of
						true -> {Updated, lists:append([{Key, Value}], Rest) };
						false -> {lists:append([{Key, Value}], Updated) , Rest}
					end
				end, {[], []}, Store).

merge(Entries, Store) ->
	lists:merge(Entries, Store).

%% ====================================================================
%% Internal functions
%% ====================================================================



%% @author pradeeppeiris
%% @doc @todo Add description to key.


-module(key).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate/0, between/3]).

%generate() ->
%	random:seed(now()),
%	random:uniform(1000000000).

generate() ->
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
                erlang:unique_integer()),
    random:uniform(1000000000).

between(Key, From, To) ->
	if
		From == To -> true;
		From < To -> (From < Key) and (Key =< To);
		From > To -> (From < Key) or (Key =< To)
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================



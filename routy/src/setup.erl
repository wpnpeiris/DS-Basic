%% @author pradeeppeiris
%% @doc @todo Add description to setup.


-module(setup).

%% ====================================================================
%% API functions
%% ====================================================================
-export([setupSweden/0, broadcastSweden/0, updateSweden/0, setupLanka/0, broadcastLanka/0, updateLanka/0]).

setupSweden() ->
	routy:start(r_stockholm, stockholm),
	timer:sleep(1000),

	routy:start(r_lund, lund),
	timer:sleep(1000),

	routy:start(r_malmo, malmo),
	timer:sleep(1000),

	r_stockholm ! {add, lund, {r_lund, 'sweden@127.0.0.1'}},
	r_lund ! {add, stockholm, {r_stockholm, 'sweden@127.0.0.1'}},

	r_stockholm ! {add, malmo, {r_malmo, 'sweden@127.0.0.1'}},
		

broadcastSweden() ->
	r_stockholm ! broadcast,
	r_lund ! broadcast,
	r_malmo ! broadcast.

updateSweden() ->
	r_stockholm ! update,
	r_lund ! update,
	r_malmo ! update.


setupLanka() ->
	routy:start(r_colombo, colombo),
	timer:sleep(1000),

	routy:start(r_negambo, negambo),
	timer:sleep(1000),

	routy:start(r_galle, galle),
	timer:sleep(1000),

	r_negambo ! {add, colombo, {r_colombo, 'lanka@127.0.0.1'}},
	r_colombo ! {add, negambo, {r_negambo, 'lanka@127.0.0.1'}},

	r_negambo ! {add, galle, {r_galle, 'lanka@127.0.0.1'}},
	r_galle ! {add, negambo, {r_negambo, 'lanka@127.0.0.1'}}.

broadcastLanka() ->
	r_colombo ! broadcast,
	r_negambo ! broadcast,
	r_galle ! broadcast.

updateLanka() ->
	r_colombo ! update,
	r_negambo ! update,
	r_galle ! update.


%% ====================================================================
%% Internal functions
%% ====================================================================



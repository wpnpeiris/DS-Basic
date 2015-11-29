%% @author pradeeppeiris
%% @doc @todo Add description to api.


-module(api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> v_time:zero().

inc(Node, T) -> v_time:inc(Node, T).

merge(Ti, Tj) -> v_time:merge(Ti, Tj).

leq(Ti,Tj)  -> v_time:leq(Ti, Tj).

clock(Nodes) -> v_time:clock(Nodes).

update(Node, Time, Clock) -> v_time:update(Node, Time, Clock).

safe(Time, Clock) -> v_time:safe(Time, Clock).

%% ====================================================================
%% Internal functions
%% ====================================================================



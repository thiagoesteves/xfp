%%%-------------------------------------------------------------------
%% @doc xfp public API
%% @end
%%%-------------------------------------------------------------------

-module(xfp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    xfp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

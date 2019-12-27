%%%-------------------------------------------------------------------
%%% Created : 27 Dec 2019 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%% This is the XFP top level supervisor where the user can create
%%% or remove xfp gen-servers.
%%% @end
%%%-------------------------------------------------------------------

-module(xfp_sup).

-behaviour(supervisor).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, create_xfp/1, remove_xfp/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(XFP_DRIVER_NAME, xfp_driver).
-define(XFP_MODULE_NAME, xfp).
-define(XFP_TIMEOUT, 5000). % ms

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 4,
                 period => 30},
    %% Xfp driver is linked at the initialisation
    ChildSpecs = [#{id => ?XFP_DRIVER_NAME,
                    start => {?XFP_DRIVER_NAME, start_link, []},
                    shutdown => brutal_kill}],
    % comment this line to stop trapping exits
    process_flag(trap_exit, true),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_xfp(Instance) ->
  XfpName = compose_xfp_name(Instance),
  XfpSpec = {XfpName, { ?XFP_MODULE_NAME, start_link, [[XfpName, Instance]]},
        permanent, ?XFP_TIMEOUT, worker, [?XFP_MODULE_NAME]},
  supervisor:start_child(?MODULE, XfpSpec).

remove_xfp(Instance) ->
  XfpName = compose_xfp_name(Instance),
  supervisor:terminate_child(?MODULE, XfpName),
  supervisor:delete_child(?MODULE, XfpName).

%%====================================================================
%% Internal functions
%%====================================================================

i2l(I) -> erlang:integer_to_list(I).
l2a(L) -> erlang:list_to_atom(L).

%% compose gen server name
compose_xfp_name(Id) ->
  Name = "Xfp:"++i2l(Id),
  l2a(Name).

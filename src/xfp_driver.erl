%%%-------------------------------------------------------------------
%%% Created : 27 Dec 2019 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is the XFP driver implementation and it is responsible to
%%% access the xfp low level functions using Port.
%%% @end
%%%-------------------------------------------------------------------

-module(xfp_driver).

-behaviour(gen_server).

-author('Thiago Esteves').

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% Xfp Driver API
-export([read_register/2,
         write_register/3,
         read_pin/2,
         write_pin/3]).

%%%===================================================================
%%% Defines
%%%===================================================================

-define(SERVER,  ?MODULE).
-define(TIMEOUT, 1000). % in milliseconds

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  Port = open_port(),
  {ok, _} = talk_to_port(Port , {open_xfp_driver}),
  {ok, Port}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_, Port) ->
  talk_to_port(Port, {close_xfp_driver}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(Msg, _From, Port) ->
  Res = talk_to_port(Port , Msg),
  {reply, Res, Port}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
open_port() ->
    E = filename:join([code:priv_dir(xfp), "xfp"]),
    open_port({spawn, E},[{packet, 2}, binary, exit_status]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
talk_to_port(Port,Msg) ->
    try
        erlang:port_command(Port, term_to_binary(Msg)),
        receive
            {Port, {data, D = <<131, 104, 2, _/binary>>}} ->
                binary_to_term(D)
        after ?TIMEOUT ->
                throw(talk_to_port_timeout)
        end
    catch
        _:R ->
            throw({talking_to_port_failed, {R, Port, Msg}})
    end.

%%%===================================================================
%%% Public API
%%%===================================================================

read_register(Instance, Register) ->
  gen_server:call(?MODULE, {read_register, Instance, Register}).

write_register(Instance, Register, Value) ->
  gen_server:call(?MODULE, {write_register, Instance, Register, Value}).

read_pin(Instance, Pin) ->
  gen_server:call(?MODULE, {read_pin, Instance, Pin}).

write_pin(Instance, Pin, Value) ->
  gen_server:call(?MODULE, {write_pin, Instance, Pin, Value}).


%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 7:20 PM
%%%-------------------------------------------------------------------
-module(agents_mgmt).
-author("elad.sofer").

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("records.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(ETS, CollectorPid, NNids, AgentIds) -> supervisor:start_link({local, ?SERVER}, ?MODULE, [ETS, CollectorPid,NNids, AgentIds]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).

init([ETS, CollectorPid, NNids, AgentIds]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},

  ZippedIds = lists:zip(NNids, AgentIds),
  AgentConstructors = [{NNId, AgentId, constructor:construct_Genotype(AgentId,rng,pts,[3,2])} || {NNId, AgentId} <- ZippedIds],
  %TODO TRANSFER LAYERS
  io:format("####~p###~n",[AgentConstructors]),

  Childs =
    [ #{id=>NNid,
      start => {'agent', start_link, [CollectorPid, ETS, NNid, SeedGene, AgentId]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => ['agent']} || {NNid, AgentId, SeedGene}<-AgentConstructors],

  {ok, {SupFlags, Childs}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================



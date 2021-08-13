%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% This module is a supervisor agents module. it responsible to make sure that
%%% Every agent is alive and able to perform it's tasks. each populationFSM
%%% Own a supervisor server. in case an agent falls, the supervisor restarts it.

%%% @end
%%% Created : 01. Aug 2021 7:20 PM
%%%-------------------------------------------------------------------
-module(agents_mgmt).
-author("elad.sofer").

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1, start_link_shell/3, generateChildrensSpecs/3]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-include("config.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================
start_link_shell(CollectorPid, NNids, AgentIds) ->
  ServerId = utills:generateServerId(?MODULE),
  {ok, Pid} = supervisor:start_link({local, ServerId}, ?MODULE, [CollectorPid,NNids, AgentIds, ServerId]),
  unlink(Pid), {ok, Pid}.

start_link(CollectorPid, NNids, AgentIds) ->
  ServerId = utills:generateServerId(?MODULE),
  supervisor:start_link({local, ServerId}, ?MODULE, [CollectorPid,NNids, AgentIds, ServerId]).

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

init([CollectorPid, NNids, AgentIds, ServerId]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},

  ChildrensSpecs =generateChildrensSpecs(NNids, AgentIds, CollectorPid),

  io:format("Hi I am Supervisor ~p~n", [ServerId]),

  {ok, {SupFlags, ChildrensSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% A function which generates the agents specs.
generateChildrensSpecs(NNids, AgentIds, CollectorPid) ->
  AgentIdsZipped = lists:zip(NNids, AgentIds),
  [#{id=>AgentId,
    start => {'agent', start_link, [CollectorPid, NNid, AgentId]},
    restart => permanent,
    shutdown => 1000,
    type => worker,
    modules => ['agent']} || {NNid, AgentId}<-AgentIdsZipped].

%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(agent).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(agent_state, {nnId, agentId, seedGene, collectorPid}).
-include("records.hrl").
%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(CollectorPid, NNid, AgentId) ->
  gen_server:start_link({local, AgentId}, ?MODULE, [CollectorPid, NNid, AgentId], []).

init([CollectorPid, NNid, AgentId]) ->

  io:format("Hi I am Agent ~p, Details: CollectorPid:~p|NNid:~p|~n", [AgentId, CollectorPid, NNid]),
  {ok, #agent_state{nnId=NNid, collectorPid=CollectorPid, agentId = AgentId}}.

handle_call({run_simulation, Gene}, _From, State = #agent_state{agentId = AgentId}) ->
  % the AgentId is: <node>_nnX atom
  {_, _, SimulationVec} = exoself:map(AgentId,Gene),
  % {SrcPid, _} = _From,
  {reply, {ok, SimulationVec}, State}.

handle_cast({executeIteration, MutId, Gene}, State = #agent_state{nnId=NNid, collectorPid=CollectorPid, agentId = AgentId}) ->
  % NNid is the AgentId as well, each agent has it's own network to handle.

  MutatedGene = mutate:mutate(Gene),
  %io:format("AgentId:~p|Gene:~p|~n",[AgentId, Gene]),
  FileName = list_to_atom("logs/" ++ atom_to_list(AgentId) ++ "_" ++integer_to_list(MutId)),

  {Score, ProcessesCount, _} = exoself:map(FileName, MutatedGene),
  %io:format("NNid:~p|Score:~p|Processes Count:~p~n",[AgentId, Score, ProcessesCount]),
  database:write(NNid,MutId,MutatedGene,ProcessesCount, Score),
  %io:format("CollectorPid:~p~n", [CollectorPid]),
  %io:format("##########################"),
  gen_statem:cast({global,CollectorPid}, {sync, AgentId}),
  {noreply, State}.


handle_info(_Info, State = #agent_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #agent_state{}) ->
  ok.

code_change(_OldVsn, State = #agent_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Agent component - this gen_server station is responsible for executing
%%% a Generation Iteration. meaning it responsible for mutating a gene to produce an offspring gene,
%%% running a Rabbit/Hunter simulation upon that offspring, and score it's result via a distance function.
%%% The agent is communicated directly with the FSM population, and transfer the results to
%%% the population pool it attached to.
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
-include("config.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link(CollectorPid, NNid, AgentId) ->
  gen_server:start_link({local, AgentId}, ?MODULE, [CollectorPid, NNid, AgentId], []).

init([CollectorPid, NNid, AgentId]) ->
  process_flag(trap_exit, true),

  io:format("Hi I am Agent ~p, Details: CollectorPid:~p|NNid:~p|~n", [AgentId, CollectorPid, NNid]),

  {ok, #agent_state{nnId=NNid, collectorPid=CollectorPid, agentId = AgentId}}.

%%%===================================================================
%%% Requests
%%%===================================================================

% A simulation request, the function raise the gene to a lively nn with processes,
% via creating the phenotype and executes the simulation of the Rabbit/Hunter
handle_call({run_simulation, Gene}, _From, State = #agent_state{agentId = AgentId}) ->
  % the AgentId is: <node>_nn<X> atom
  {_, _, SimulationVec} = phenotype_gen:bringGeneToLife(AgentId,Gene),
  {reply, {ok, SimulationVec}, State}.

% this is the request for generating a new offspring and send the scores, the amount of processes
% the gene offspring produced.
handle_cast({executeIteration, MutId, Gene}, State = #agent_state{nnId=NNid, collectorPid=CollectorPid, agentId = AgentId}) ->
  % mutate the gene to produce an offspring
  MutatedGene = mutation_gen:mutate(Gene),
  % log file for tracking the created genes.
  FileName = list_to_atom("logs/" ++ atom_to_list(AgentId) ++ "_" ++integer_to_list(MutId)),
  % bring to life the gene
  {Score, ProcessesCount, _} = phenotype_gen:bringGeneToLife(FileName, MutatedGene),

  database:write(NNid,MutId,MutatedGene,ProcessesCount, Score),
  gen_statem:cast({global, CollectorPid}, {sync, AgentId}),
  {noreply, State}.


handle_info(_Info, State = #agent_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #agent_state{}) ->
  ok.

code_change(_OldVsn, State = #agent_state{}, _Extra) ->
  {ok, State}.

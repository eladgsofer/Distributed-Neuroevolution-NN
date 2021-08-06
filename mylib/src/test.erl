%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-include("records.hrl").

-compile(export_all).
%Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,Nodes

test()-> db:init(),
  Layers=[4],
  Max_Mutation_iterations=1,
  Simulation_steps=1,
  NN_amount=3,
  Rabbit_pos={1,1},
  Nodes={n,n,n},
  %{NNids, AgentsIds} = master_server:generate_seeds(NN_amount,Layers),
  %io:format("NNid:~p~n", [NNids]),
  %io:format("NNid:~p~n", [AgentsIds]),
  %population_fsm:start_link(NN_amount, Simulation_steps, self(), {NNids, AgentsIds}).
  master_server:start_link(Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,Nodes).


test1() ->
  ETS = ets:new(tab, [set]),
  G = constructor:construct_Genotype(nn1,rng,pts, [4]),
  io:format("BEFORE:~n~p~n", [G]),
  MutatedGene = mutate:mutate(G),
  io:format("AFTER:~n~p~n", [MutatedGene]),
  exoself:map(nn1,MutatedGene).

test_agent_sim()->
  G = constructor:construct_Genotype(nn1,rng,pts, [4]),
  agent:start_link(self(), nonode@nohost, agent1),
  RES = gen_server:call(agent1, {run_simulation, G}),
  io:format("Wait for message... my pid is~p~n", [self()]),
  receive
    Msg -> io:format("RX!"), {Msg, RES}
  end.

test_agent_iter()->
  G = constructor:construct_Genotype(nn1,rng,pts, [4]),
  agent:start_link(self(), nonode@nohost, agent1),
  RES = gen_server:cast(agent1, {executeIteration, 1, G}),
  io:format("Wait for message... my pid is~p~n", [self()]).

test_supervisor(NN_Amount)->
  db:init([]),
  NNnames = [list_to_atom("nn" ++ integer_to_list(N)) || N<-lists:seq(1,NN_Amount)],
  NNids = [{node(), Name} || Name<-NNnames],
  AgentsIds = [list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Id)) || {Node, Id}<-NNids],
  AgentsMapper = maps:from_list([{A, false} ||A<-AgentsIds]),
  agents_mgmt:start_link_shell(self(), NNids, AgentsIds).

generateServerId()-> list_to_atom(atom_to_list(node()) ++ "_" ++ atom_to_list(?MODULE)).

%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Unit testing module
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-include("records.hrl").
-include("config.hrl").
-record(test,{name1,name2}).
-compile(export_all).

%%%-------------------------------------------------------------------

master()->

  Layers=[4,8,5],
  Max_Mutation_iterations=20,
  NN_amount=4*3*50, % A NUMBER WHICH DIVIDES BY NUMBER OF NODES

  master_server:start_king(Layers,Max_Mutation_iterations,NN_amount).

slave()->
  Layers=[4,8,5],
  Max_Mutation_iterations=20,
  NN_amount=4*3*50, % A NUMBER WHICH DIVIDES BY NUMBER OF NODES

  master_server:start_slave(Layers,Max_Mutation_iterations,NN_amount).

test_supervisor(NN_Amount)->
  % Crash example
  % erl -sname king
  % gen_server:cast('king@Tom-VirtualBox_nn1', crash_test).
  NNnames = [list_to_atom("nn" ++ integer_to_list(N)) || N<-lists:seq(1,NN_Amount)],
  NNids = [{node(), Name} || Name<-NNnames],
  AgentsIds = [list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Id)) || {Node, Id}<-NNids],
  agents_mgmt:start_link_shell(self(), NNids, AgentsIds).

%%%-------------------------------------------------------------------


test1() ->
  ETS = ets:new(tab, [set]),
  G = genotype_gen:construct_Genotype(nn1,rng,pts, [4]),
  io:format("BEFORE:~n~p~n", [G]),
  MutatedGene = mutation_gen:mutate(G),
  io:format("AFTER:~n~p~n", [MutatedGene]),
  phenotype_gen:bringGeneToLife(nn1,MutatedGene).

test_agent_iter()->
  G = genotype_gen:construct_Genotype(nn1,rng,pts, [4]),
  agent:start_link(self(), nonode@nohost, agent1),
  RES = gen_server:cast(agent1, {executeIteration, 1, G}),
  io:format("Wait for message... my pid is~p~n", [self()]).



generateServerId()-> list_to_atom(atom_to_list(node()) ++ "_" ++ atom_to_list(?MODULE)).

tom()->  G = genotype_gen:construct_Genotype(nn1,rng,pts, [4,5,8,6,7]),
  MutatedGene = mu(G,1000),
  io:format("Finish Mutate:~p~n",[MutatedGene]),
  phenotype_gen:bringGeneToLife(nn1,MutatedGene).

mu(G,0)->G;
mu(G,Cnt)->mu(mutation_gen:mutate(G),Cnt-1).

gen()-> genotype_gen:construct_Genotype(nn1,rng,pts, [4,3]).


test_gui()-> L=[[1,1,3,3],
  [2,2,2,4],
  [3,3,2,5],
  [4,4,2,6],
  [5,5,2,7],
  [6,6,2,8],
  [7,7,2,9],
  [8,8,1,10],
  [9,9,1,11]],
  graphic:start(),
  Statistics = [{process, 10},{neurons, 10},{fitness, 10}],
  master_server:display(L,Statistics).



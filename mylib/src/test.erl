%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-include("records.hrl").
-record(test,{name1,name2}).
-compile(export_all).
%Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,Nodes

master()->

  Layers=[4,8,5],
  Max_Mutation_iterations=20,
  Simulation_steps=5000,
  NN_amount=4*3*50, % A NUMBER WHICH DIVIDES BY NUMBER OF NODES

  master_server:start_king(Layers,Max_Mutation_iterations,Simulation_steps,NN_amount).

slave()->
  Layers=[4,8,5],
  Max_Mutation_iterations=20,
  Simulation_steps=5000,
  NN_amount=4*3*50, % A NUMBER WHICH DIVIDES BY NUMBER OF NODES

  master_server:start_slave(Layers,Max_Mutation_iterations,Simulation_steps,NN_amount).

test1() ->
  ETS = ets:new(tab, [set]),
  G = constructor:construct_Genotype(nn1,rng,pts, [4]),
  io:format("BEFORE:~n~p~n", [G]),
  MutatedGene = mutate:mutate(G),
  io:format("AFTER:~n~p~n", [MutatedGene]),
  exoself:map(nn1,MutatedGene).

test_agent_sim()->
  G = [{cortex,
    {cortex,6.141167187141267e-10},
    [{sensor,6.141167187141274e-10}],
    [{actuator,6.14116718714127e-10}],
    [{neuron,{2,6.14116718705156e-10}},
      {neuron,{1,6.141167187141261e-10}},
      {neuron,{1,6.141167187141258e-10}},
      {neuron,{1,6.141167187141254e-10}},
      {neuron,{1,6.141167187141251e-10}},
      {neuron,{2,6.141167187141247e-10}},
      {neuron,{2,6.141167187141243e-10}},
      {neuron,{3,6.141167187141191e-10}},
      {neuron,{3,6.141167187141186e-10}}]},
    {sensor,
      {sensor,6.141167187141274e-10},
      {cortex,6.141167187141267e-10},
      rng,4,
      [{neuron,{1,6.141167187141261e-10}},
        {neuron,{1,6.141167187141258e-10}},
        {neuron,{1,6.141167187141254e-10}},
        {neuron,{1,6.141167187141251e-10}}]},
    {actuator,
      {actuator,6.14116718714127e-10},
      {cortex,6.141167187141267e-10},
      pts,2,
      [{neuron,{3,6.141167187141191e-10}},
        {neuron,{3,6.141167187141186e-10}}]},
    {neuron,
      {neuron,{1,6.141167187141251e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{sensor,6.141167187141274e-10},
        [-0.3437500119676534,0.3327898443207148,
          -0.11609509928941397,0.3310335340304996]},
        {bias,0.17587091747036776}],
      [{neuron,{2,6.141167187141243e-10}},
        {neuron,{2,6.141167187141247e-10}}]},
    {neuron,
      {neuron,{2,6.14116718705156e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{bias,0.43545590166719306}],                     %TODO
      [{neuron,{3,6.141167187141186e-10}}]},
    {neuron,
      {neuron,{1,6.141167187141261e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{sensor,6.141167187141274e-10},
        [0.4561039722647726,0.028238069241967922,
          0.37759407503601594,-0.25703822584392066]},
        {bias,0.3873255335254714}],
      [{neuron,{2,6.141167187141243e-10}},
        {neuron,{2,6.141167187141247e-10}}]},
    {neuron,
      {neuron,{3,6.141167187141191e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{neuron,{2,6.141167187141243e-10}},
        [-0.2217923211476177]},
        {{neuron,{2,6.141167187141247e-10}},
          [-0.09171710703909353]},
        {bias,-0.3227134407402381}],
      [{actuator,6.14116718714127e-10}]},
    {neuron,
      {neuron,{2,6.141167187141243e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{neuron,{1,6.141167187141251e-10}},
        [-0.29209438038637314]},
        {{neuron,{1,6.141167187141254e-10}},
          [0.4187162021483639]},
        {{neuron,{1,6.141167187141258e-10}},
          [-0.1789243955213916]},
        {{neuron,{1,6.141167187141261e-10}},
          [0.23796012356105378]},
        {bias,0.4258926926936458}],
      [{neuron,{3,6.141167187141186e-10}},
        {neuron,{3,6.141167187141191e-10}}]},
    {neuron,
      {neuron,{1,6.141167187141254e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{sensor,6.141167187141274e-10},
        [-0.19754354210605285,-0.2574736083189145,
          0.3339261281906636,-0.28002284694479096]},
        {bias,0.49076786571427833}],
      [{neuron,{2,6.141167187141243e-10}},
        {neuron,{2,6.141167187141247e-10}}]},
    {neuron,
      {neuron,{3,6.141167187141186e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{neuron,{2,6.14116718705156e-10}},
        [-0.12889423197494665]},
        {{neuron,{2,6.141167187141243e-10}},
          [0.48005215315614125]},
        {{neuron,{2,6.141167187141247e-10}},
          [0.3588987337721907]},
        {bias,0.03290286312960644}],
      [{actuator,6.14116718714127e-10}]},
    {neuron,
      {neuron,{1,6.141167187141258e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{sensor,6.141167187141274e-10},
        [0.32762605079814044,0.04761789678859363,
          -0.23081771357256498,0.12783089351868127]},
        {bias,-0.05128678910388007}],
      [{neuron,{2,6.141167187141243e-10}}]},
    {neuron,
      {neuron,{2,6.141167187141247e-10}},
      {cortex,6.141167187141267e-10},
      tanh,
      [{{neuron,{1,6.141167187141251e-10}},
        [0.4313907990931143]},
        {{neuron,{1,6.141167187141254e-10}},
          [-0.23074074023369462]},
        {{neuron,{1,6.141167187141261e-10}},
          [-0.2925077864894612]},
        {bias,0.1578607353528323}],
      [{neuron,{3,6.141167187141186e-10}},
        {neuron,{3,6.141167187141191e-10}}]}],
  exoself:map(test, G).
  %agent:start_link(self(), nonode@nohost, agent1),
  %RES = gen_server:call(agent1, {run_simulation, G}),
  %io:format("Wait for message... my pid is~p~n", [self()]),
  %receive
   % Msg -> io:format("RX!"), {Msg, RES}
  %end.

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

tom()->  G = constructor:construct_Genotype(nn1,rng,pts, [4,5,8,6,7]),
  MutatedGene = mu(G,1000),
  io:format("Finish Mutate:~p~n",[MutatedGene]),
  exoself:map(nn1,MutatedGene).

mu(G,0)->G;
mu(G,Cnt)->mu(mutate:mutate(G),Cnt-1).

gen()-> constructor:construct_Genotype(nn1,rng,pts, [4,3]).










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



%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2021 3:44 PM
%%%-------------------------------------------------------------------
-module(master_server).
-author("tom").
-include("records.hrl").

-behaviour(gen_server).

%% API
-export([start_link/6]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_cast/3,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {master,node1,node2,node3,nn_amount,mutate_iteration,max_mutate_iteration,rabbit_pos}).
-record(track,{master,node1,node2,node3}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,Nodes) ->    %Nods={node1,node2,node3}
  gen_server:start_link({global, king}, ?MODULE, [Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,Nodes],[]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,{Node1,Node2,Node3}]) ->
  db:init(),        %for simulation
  %db:init([Node1,Node2,Node3]),
  #state{master = node(),node1 =Node1,node2 = Node2,node3 = Node3,nn_amount = NN_amount,
    mutate_iteration=0,max_mutate_iteration = Max_Mutation_iterations,rabbit_pos = Rabbit_pos},
  graphic:start(),#track{master = maps:new(),node1 = maps:new(),node2 = maps:new(),node3 = maps:new()},
  Env_Params = generate_seeds(NN_amount,Layers),
  population_fsm:start_link(NN_amount,Simulation_steps,self(), Env_Params),
  timer:send_interval(80, self(), check_genotyps),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({ok,NNids}, _From)->
  From = utills:generateServerId(node(),master_process),
  To = utills:generateServerId(node(),population_fsm),
  Seeds = [{NNid,0}||NNid<-NNids],
  {To,node()} ! {runNetwork, Seeds, 0}.
  %gen_server:cast({To,From}, {runNetwork, Seeds, 0}).

handle_cast({#state.master,done, Mutation_iterations}, _From, State) ->
  Old = #track.master, #track{master=maps:put(Mutation_iterations, finish, Old)},
  {reply, ok, State};

handle_cast({#state.node1,done, Mutation_iterations}, _From, State) ->
  Old = #track.node1, #track{node1=maps:put(Mutation_iterations, finish, Old)},
  {reply, ok, State};

handle_cast({#state.node2,done, Mutation_iterations}, _From, State) ->
  Old = #track.node2, #track{node2=maps:put(Mutation_iterations, finish, Old)},
  {reply, ok, State};

handle_cast({#state.node3,done, Mutation_iterations}, _From, State) ->
  Old = #track.node3, #track{node3=maps:put(Mutation_iterations, finish, Old)},
    {reply, ok, State}.


handle_info(_Info, State) ->
  Mutate_iteration = #state.mutate_iteration,
  Node_list = [{#track.master,#state.master},{#track.node1,#state.node1},{#track.node2,#state.node2},{#track.node3,#state.node3}],
  Active_Nodes = [{Node_Map,Node}||{Node_Map,Node}<-Node_list,net_adm:ping(Node)==pong],
  Readys = [ready||{Node_Map,_}<-Active_Nodes,(maps:find(Mutate_iteration,Node_Map)==error)==false],
  case length(Readys) == length(Active_Nodes) of
  false-> {noreply, State};
  true-> case Mutate_iteration==#state.max_mutate_iteration of
            false->#state{mutate_iteration= Mutate_iteration+1},calc(Mutate_iteration,Active_Nodes),{noreply, State};
            true-> {Best_score,Best_Genotype,Processes_cnt} = choose_best(Mutate_iteration),
              Cx = hd(Best_Genotype),
              Nurons_num = length(Cx#cortex.nids),
              Statistics = [{process, Processes_cnt},{neurons, Nurons_num},{fitness, Best_score}],
              Hunter_path = gen_server:call(agent1,{run_simulation,Best_Genotype}),
              display(Hunter_path,#state.rabbit_pos,Statistics),{noreply, State}
          end
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
calc(Mutation_iterations,Active_Nodes)-> %%mnesia:force_load_table(db),
  {atomic,List} = db:read_all_mutateIter(Mutation_iterations),
  Filtered = [{Score,{NNid,MutatIter}}||{db,NNid,MutatIter,_,_,Score} <-List],
  Sortd_by_score = lists:keysort(2,Filtered),Bests_genotyps = lists:sublist(Sortd_by_score,#state.nn_amount),brodcast_genotyps(Bests_genotyps,Active_Nodes).

brodcast_genotyps(Bests_genotyps,Active_Nodes)-> Choden_Genes = [{NNid,MutatIter}||{_,{NNid,MutatIter}} <-Bests_genotyps],
  Node_Names = [utills:generateServerId(Node,master_process)||{_,Node}<-Active_Nodes],
  lists:foreach(fun(Node_name)-> gen_server:cast({Node_name, node()}, {runNetwork, Choden_Genes, #state.mutate_iteration}) end,Node_Names).

choose_best(Mutation_iterations)->%%mnesia:force_load_table(db),
  {atomic,List} = db:read_all_mutateIter(Mutation_iterations),
  Filtered = [{Score,{Genotype,Processes_cnt}}||{db,_,_,Genotype,Processes_cnt,Score} <-List],
  Sortd_by_score = lists:keysort(2,Filtered), {Score,{Genotype,Processes_cnt}}=hd(Sortd_by_score),
  {Score,Genotype,Processes_cnt}.

display([],[],_)-> ok;
display([Hunter_Pos|Hun_Positions],[Rabbit_Pos|Rab_Positions],Statistics)->
  graphic:update_location(Rabbit_Pos,Hunter_Pos,Statistics),
  display(Hun_Positions,Rab_Positions,Statistics).


generate_seeds(NN_amount,Layers)-> % Initialize State
  NNnames = [list_to_atom("nn" ++ integer_to_list(N)) || N<-lists:seq(1,NN_amount)], % nn1, nn2...
  NNids = [{node(), Name} || Name<-NNnames], % [{node(), nn1}]...
  AgentsIds = [list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Id)) || {Node, Id}<-NNids], % [node1_nn1, node2_nn2]...
  Env_Params = {NNids, AgentsIds},
  Seeds=[#db{nn_id = NNid,mutId =0,
    gene=constructor:construct_Genotype(AgentId,rng,pts,Layers),
    processes_count = 0,score = 0}||AgentId<-AgentsIds,NNid <-NNids],
  db:write_records(Seeds),Env_Params.



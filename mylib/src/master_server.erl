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
  handle_cast/3,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {master_node,node1,node2,node3,nn_amount,mutate_iteration,max_mutate_iteration,rabbit_pos}).
-record(genotyps,{master,node1,node2,node3}).

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
  gen_server:start_link({global, king}, ?MODULE, [], [Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,Nodes]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Layers,Max_Mutation_iterations,Simulation_steps,NN_amount,Rabbit_pos,{Node1,Node2,Node3}]) ->
  db:init(Node1,Node2,Node3),
  #state{master_node = node(),node1 =Node1,node2 = Node2,node3 = Node3,nn_amount = NN_amount,
    mutate_iteration=0,max_mutate_iteration = Max_Mutation_iterations,rabbit_pos = Rabbit_pos},
  graphic:start(), population_fsm:start_link(Layers,Max_Mutation_iterations,Simulation_steps),
  ,#genotyps{master = maps:new(),node1 = maps:new(),node2 = maps:new(),node3 = maps:new()},
  timer:send_interval(80, self(), check_genotyps), {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({#state.master_node,Mutation_iterations,Ets}, _From, State) ->
  Old = #genotyps.master, #genotyps{master=maps:put(Mutation_iterations, Ets, Old)},
  {reply, ok, State};

handle_cast({#state.node1,Mutation_iterations,Ets}, _From, State) ->
  Old = #genotyps.node1, #genotyps{node1=maps:put(Mutation_iterations, Ets, Old)},
  {reply, ok, State};

handle_cast({#state.node2,Mutation_iterations,Ets}, _From, State) ->
  Old = #genotyps.node2, #genotyps{node2=maps:put(Mutation_iterations, Ets, Old)},
  {reply, ok, State};

handle_cast({#state.node3,Mutation_iterations,Ets}, _From, State) ->
  Old = #genotyps.node3, #genotyps{node3=maps:put(Mutation_iterations, Ets, Old)},
    {reply, ok, State}.


handle_info(_Info, State) ->
  Mutate_iteration = #state.mutate_iteration,
  Monitor1 = gen_statem:cast(ping,self()),        %TODO
  Monitor2 = gen_statem:cast(ping,self()),
  Monitor3 = gen_statem:cast(ping,self()),
  Bol0 = maps:find(Mutate_iteration,#genotyps.master)==error,
  Bol1 = maps:find(Mutate_iteration,#genotyps.node1)==error,
  Bol2 = maps:find(Mutate_iteration,#genotyps.node2)==error,
  Bol3 = maps:find(Mutate_iteration,#genotyps.node3)==error,
  case Bol0 or (Bol1  and Monitor1) or (Bol2 and Monitor2) or (Bol3 and Monitor3) of
  true-> {noreply, State};
  false-> case Mutate_iteration==#state.max_mutate_iteration of
            false->calc(Mutate_iteration),#state{mutate_iteration= Mutate_iteration+1},{noreply, State};
            true-> {Best_score,Best_Genotype} = choose_best(Mutate_iteration),
              Nurons_num = length(hd(Best_Genotype)#cortex.nids),
              Statistics = [{process, 1000},{neurons, Nurons_num},{fitness, Best_score}],       %TODO get num of process
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
calc(Mutation_iterations)->
  All_score_genotyps =ets:tab2list(maps:get(Mutation_iterations,#genotyps.master))++ets:tab2list(maps:get(Mutation_iterations,#genotyps.node1))++
    ets:tab2list(maps:get(Mutation_iterations,#genotyps.node2))++ets:tab2list(maps:get(Mutation_iterations,#genotyps.node3)),
  Sortd_by_score = lists:keysort(2,All_score_genotyps),Bests_genotyps = lists:sublist(Sortd_by_score,#state.nn_amount),brodcast_genotyps(Bests_genotyps).

brodcast_genotyps(Bests_genotyps)->T = ets:new(best,[bag]),lists:foreach(fun({_,Genotype})-> ets:insert(T,Genotype) end,Bests_genotyps),
  {#state.node1,population} ! T, {#state.node2,population} ! T, {#state.node3,population} ! T,{#state.master_node,population} ! T.

choose_best(Mutation_iterations)->All_score_genotyps =ets:tab2list(maps:get(Mutation_iterations,#genotyps.master))++ets:tab2list(maps:get(Mutation_iterations,#genotyps.node1))++
  ets:tab2list(maps:get(Mutation_iterations,#genotyps.node2))++ets:tab2list(maps:get(Mutation_iterations,#genotyps.node3)),
  Sortd_by_score = lists:keysort(2,All_score_genotyps),Best=hd(Sortd_by_score),Best.

display([],[],_)-> ok;
display([Hunter_Pos|Hun_Positions],[Rabbit_Pos|Rab_Positions],Statistics)->
  graphic:update_location(Rabbit_Pos,Hunter_Pos,Statistics),
  display(Hun_Positions,Rab_Positions,Statistics).




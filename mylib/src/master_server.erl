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
-export([start_link/6,generate_seeds/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MASTER_NODE, 'master@Tom-VirtualBox').
-define(NODE1, none1).
-define(NODE2, none2).
-define(NODE3, none3).

-record(state, {nn_amount,mutate_iteration,max_mutate_iteration,rabbit_pos, track,prev_nodes}).
-record(track,{?MASTER_NODE,?NODE1,?NODE2,?NODE3}).

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
  Track = #track{?MASTER_NODE = {?MASTER_NODE,maps:new()}, ?NODE1 = {?NODE1,maps:new()},?NODE2 = {?NODE2,maps:new()},?NODE3 = {?NODE3,maps:new()}},

  State = #state{nn_amount = NN_amount,mutate_iteration=1,max_mutate_iteration = Max_Mutation_iterations,
    rabbit_pos = Rabbit_pos, track=Track,prev_nodes = monitorNodes()},

  graphic:start(),
  {NNids, AgentsIds} = generate_seeds(NN_amount,Layers),
  population_fsm:start_link(NN_amount,Simulation_steps,self(),{NNids, AgentsIds}),
  timer:send_interval(80, self(), check_genotyps),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({ok,Node, NNids},State)->
  Servername = utills:generateServerId(Node,population_fsm),
  Seeds = [{NNid,0}||NNid<-NNids],
  gen_statem:cast(Servername,{runNetwork, Seeds, 1}),
  {noreply, State};

handle_cast({?MASTER_NODE, done, Mutation_iterations}, State) ->
  Old = State#state.track,
  {_,TrackNode} = Old#track.?MASTER_NODE,
  NewTrack = Old#track{?MASTER_NODE = {?MASTER_NODE,maps:put(Mutation_iterations, finish, TrackNode)}},
  {noreply, State#state{track = NewTrack}};

handle_cast({?NODE1, done, Mutation_iterations},State) ->
  Old = State#state.track,
  {_,TrackNode} = Old#track.?NODE1,
  NewTrack = Old#track{?NODE1 = {?NODE1,maps:put(Mutation_iterations, finish, TrackNode)}},
  {noreply, State#state{track = NewTrack}};

handle_cast({?NODE2,done, Mutation_iterations},State) ->
  Old = State#state.track,
  {_,TrackNode} = Old#track.?NODE2,
  NewTrack = Old#track{?NODE2 = {?NODE2,maps:put(Mutation_iterations, finish, TrackNode)}},
  {noreply, State#state{track = NewTrack}};

handle_cast({?NODE3,done, Mutation_iterations}, State) ->
  Old = State#state.track,
  {_,TrackNode} = Old#track.?NODE3,
  NewTrack = Old#track{?NODE3 = {?NODE3,maps:put(Mutation_iterations, finish, TrackNode)}},
  {noreply, State#state{track = NewTrack}}.


handle_info(_Info, State) ->
  Mutate_iteration = State#state.mutate_iteration,
  Active_Nodes= monitorNodes(),
  case Active_Nodes==State#state.prev_nodes of
    true-> handleIteration(State,Active_Nodes,Mutate_iteration);
    false -> State#state{prev_nodes = Active_Nodes}, restartIteration()
  end,
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handleIteration(State,Active_Nodes,Mutate_iteration) ->
  Track = State#state.track, Maps = tl(tuple_to_list(Track)),
  io:format("Map:~p~n", [Maps]),  io:format("Active_Nodes:~p~n", [Active_Nodes]),
  Readys = [ready||Node <-Active_Nodes, {_,Map} =lists:keyfind(Node, 1, Maps),
    (maps:find(Mutate_iteration,Map)==error)==false],
  io:format("Readys:~p~n", [Readys]),
  case length(Readys)==length(Active_Nodes) of
    true->  io:format("handleIteration:~n"),io:format("Active_Nodes:~p~n", [Active_Nodes]),
     io:format("Mutate_iteration:~p~n", [Mutate_iteration]),
      case Mutate_iteration==State#state.max_mutate_iteration of
             false->State#state{mutate_iteration= Mutate_iteration+1},calc(Mutate_iteration,Active_Nodes),{noreply, State};
             true-> {Best_score,Best_Genotype,Processes_cnt} = choose_best(Mutate_iteration),
               Cx = hd(Best_Genotype),
               Nurons_num = length(Cx#cortex.nids),
               Statistics = [{process, Processes_cnt},{neurons, Nurons_num},{fitness, Best_score}],
               Agent = utills:generateServerId(?MASTER_NODE, nn1),
               Hunter_path = gen_server:call(Agent,{run_simulation,Best_Genotype}),
               display(Hunter_path,State#state.rabbit_pos,Statistics),{noreply, State}
           end;
    false-> {noreply, State}
  end.


calc(Mutation_iterations,Active_Nodes)-> %%mnesia:force_load_table(db),
  {atomic,List} = db:read_all_mutateIter(Mutation_iterations),
  Filtered = [{Score,{NNid,MutatIter}}||{db,NNid,MutatIter,_,_,Score} <-List],
  Sortd_by_score = lists:keysort(1,Filtered),Bests_genotyps = lists:sublist(Sortd_by_score,#state.nn_amount),brodcast_genotyps(Bests_genotyps,Active_Nodes).

brodcast_genotyps(Bests_genotyps,Active_Nodes)-> Choden_Genes = [{NNid,MutatIter}||{_,{NNid,MutatIter}} <-Bests_genotyps],
  Node_Names = [utills:generateServerId(Node,?MODULE)||Node<-Active_Nodes],
  lists:foreach(fun(Node_name)-> gen_server:cast(Node_name, {runNetwork, Choden_Genes, #state.mutate_iteration}) end,Node_Names).

choose_best(Mutation_iterations)->%%mnesia:force_load_table(db),
  {atomic,List} = db:read_all_mutateIter(Mutation_iterations),
  Filtered = [{Score,{Genotype,Processes_cnt}}||{db,_,_,Genotype,Processes_cnt,Score} <-List],
  Sortd_by_score = lists:keysort(1,Filtered), {Score,{Genotype,Processes_cnt}}=hd(Sortd_by_score),
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
  AgentIdsZipped = lists:zip(NNids, AgentsIds),
  Seeds=[#db{nn_id = NNid,mutId =0,
    gene=constructor:construct_Genotype(AgentId,rng,pts,Layers),
    processes_count = 0,score = 0}||{NNid, AgentId}<-AgentIdsZipped],
  db:write_records(Seeds),Env_Params.


monitorNodes()->
  NodeList = [?MASTER_NODE, ?NODE1, ?NODE2, ?NODE3],
  [Node || Node<- NodeList, net_adm:ping(Node)==pong].

restartIteration()->elad.
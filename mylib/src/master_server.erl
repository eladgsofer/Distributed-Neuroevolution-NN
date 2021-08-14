%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%% Created : 04. Aug 2021 3:44 PM
%%%-------------------------------------------------------------------
-module(master_server).
-author("tom").

-behaviour(gen_server).

%% API
-export([start_link/4, generateSeeds/2, start_slave/3, start_king/3, display/2, findSlaves/0, findActiveNodes/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("config.hrl").
-include("records.hrl").

-define(SERVER, ?MODULE).

-define(TIMER_INTERVAL, 1000).

-record(state, {nn_amount,nnPerNode,mutate_iteration,max_mutate_iteration, track,prev_nodes, timer_ref, parentGenes, pop_fsm_pid, systemStopped, simulationPath}).
-record(track,{?MASTER_NODE,?NODE1,?NODE2,?NODE3}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%%%% The master_server is a gen_server OTP which responsible for running the whole show.
%%%%% it is responsible for triggering the all the Population FSM's, gather all the Genes scores
%%%%% select the best Offsprings and supervise the system. in case a node is down, the master
%%%%% changes the PopulationFSM's workload, and change the population NN amount.
%%%%% if a master_server is being called as "king" it is responsible for the GUI as well,
%%%%% otherwise it just trigger's it's own population FSMs.
%% @end
%% ---------------------------------------------------
% Start a king master_Server
start_king(Layers,Max_Mutation_iterations,NN_amount)->
  start_link(Layers,Max_Mutation_iterations,NN_amount,true).
% Start a king master_Server
start_slave(Layers,Max_Mutation_iterations,NN_amount)->
  start_link(Layers,Max_Mutation_iterations,NN_amount,false).

start_link(Layers,Max_Mutation_iterations,NN_amount,IsMaster) ->
  ServerId =
    case IsMaster of
      true  -> king;
      false-> utills:generateServerId(?MODULE)
    end,

  gen_server:start_link({global, ServerId}, ?MODULE, [Layers,Max_Mutation_iterations,NN_amount,IsMaster, ServerId], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% the init has a different role depending on king/slave flag.
%% slave role - wait for the master to init the mnesia service, and start commanding the FSM machine
%% king role - init the node's mnesia service, start the GUI, command it's FSM. sync the populations' FSMs.
%% and trigger the rest of the population FSM's to calculate the next generation.
%%
%% @end
%% ---------------------------------------------------

init([Layers,Max_Mutation_iterations,NN_amount, IsMaster, ServerId]) ->
  % avoid a bug.
  register(ServerId, self()),

  Track = #track{?MASTER_NODE = {?MASTER_NODE,maps:new()}, ?NODE1 = {?NODE1,maps:new()},?NODE2 = {?NODE2,maps:new()},?NODE3 = {?NODE3,maps:new()}},

  State = #state{nn_amount = NN_amount, mutate_iteration=1, max_mutate_iteration = Max_Mutation_iterations, systemStopped = false,
    track=Track, prev_nodes = findActiveNodes(), parentGenes = []},
  NewState =
    case IsMaster of
      %% ---------------- master_server AS KING
      true->
        % find the Active nodes and slaves
        ActiveNodes = findActiveNodes(),
        ActiveSlaves = findSlaves(),
        % init the DB in remote nodes
        database:createDBSchema(ActiveNodes),
        % Trigger Slaves - They start the Mnesia
        lists:foreach(fun(Sl_Node)-> Dest = {utills:generateServerId(Sl_Node, ?MODULE), Sl_Node}, io:format("Dest~p", [Dest]) , Dest ! {king, startSlave} end, ActiveSlaves),
        collectMnesiaStartMsgs(ActiveSlaves),
        database:init(ActiveNodes),
        % calculate FSM workload
        NNPerNode = round(math:floor(NN_amount/length(findActiveNodes()))),
        % start FSM and GUI.
        {ok, PopFSM_PId} = startPopulationFSM(self(), NNPerNode, Layers, ?SIM_ITERATIONS),
        graphic:start(),
        % Trigger FSM's status polling timer
        {ok, TimerRef} = timer:send_interval(?TIMER_INTERVAL, self(), check_genotyps),
        State#state{timer_ref=TimerRef, nnPerNode = NNPerNode, pop_fsm_pid =  PopFSM_PId};
      %% ---------------- master_server AS slave
      false ->
        % Wait that the master init the Mnesia DB
        io:format("Waiting for master...~n"),
        receive
          {king, startSlave} -> mnesia:start(), {king, ?MASTER_NODE} ! {node(), mnesiaStarted}
        end,
        % Calculate the workload
        NNPerNode = round(math:floor(NN_amount/length(findActiveNodes()))),
        MasterId = {king, ?MASTER_NODE},
        io:format("STARTING POPULATION~n"),
        % start the FSM
        {ok, PopFSM_PId} = startPopulationFSM(MasterId, NNPerNode, Layers, ?SIM_ITERATIONS),
        State#state{nnPerNode = NNPerNode, pop_fsm_pid =  PopFSM_PId}
    end, {ok, NewState}.



handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({ok, Node, NNids},State)->
  io:format("Population IS UP ~p~n", [Node]),
  Servername = utills:generateServerId(Node,population_fsm),
  Seeds = [{NNid,0}||NNid<-NNids],
  gen_statem:cast({global, Servername}, {runNetwork, Seeds, 1}),
  {noreply, State};

% Update which Population FSM finished to create it's population generation,
% when all the Population FSM's finished, a new generation will born.
handle_cast({Node, done, Mutation_iterations}, State) ->
  io:format("Done from:~p~n", [Node]),
  Old = State#state.track,
  NewTrack = case Node of
               ?MASTER_NODE->
                 {_,TrackNode} = Old#track.?MASTER_NODE,
                 Old#track{?MASTER_NODE = {?MASTER_NODE,maps:put(Mutation_iterations, finish, TrackNode)}};
               ?NODE1->
                 {_,TrackNode} = Old#track.?NODE1,
                 Old#track{?NODE1 = {?NODE1,maps:put(Mutation_iterations, finish, TrackNode)}};
               ?NODE2->
                 {_,TrackNode} = Old#track.?NODE2,
                 Old#track{?NODE2 = {?NODE2,maps:put(Mutation_iterations, finish, TrackNode)}};
               ?NODE3->
                 {_,TrackNode} = Old#track.?NODE3,
                 Old#track{?NODE3 = {?NODE3,maps:put(Mutation_iterations, finish, TrackNode)}};
               _-> ok
             end,
  {noreply, State#state{track = NewTrack}}.

% The timer callback function, this function verifies if all the node's FSM's finished their offspring gene and scoring creation,
% and if everyone finished it picks the best genes from the offsprings and parents and trigger a new generation creation if necessary.
% when max_mutation is reached, the best gene Graphic simulation is being displayed in the GUI.
handle_info(_Info, State) ->
  Mutate_iteration = State#state.mutate_iteration,
  NN_Amount = State#state.nn_amount,
  Active_Nodes= findActiveNodes(),
  %CHANGE NUMBER OF NN_AMOUNT FOR EACH NODE
  NewState = case Active_Nodes==State#state.prev_nodes of
               true->
                 handleIteration(State, Active_Nodes, Mutate_iteration);
               false ->
                 % Restart the current iteration, and rebalance node's workload.
                 io:format("#### NODES CHANGED ACTIVE:~p PREV:~p~n", [Active_Nodes, State#state.prev_nodes]),
                 % Update the work per node equally
                 Updated_State_1 = State#state{prev_nodes = Active_Nodes, nnPerNode = round(math:floor(NN_Amount/length(Active_Nodes)))},
                 Updated_State_1

             end,

  {noreply, NewState}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% start the PopulationFSM gen_statem
startPopulationFSM(MasterId, NNPerNode,Layers, Simulation_steps)->
  {NNids, AgentsIds} = generateSeeds(NNPerNode,Layers),
  population_fsm:start_link(NNPerNode,Simulation_steps, MasterId,{NNids, AgentsIds}).
%%%-------------------------------------------------------------------
% This function handles a generation iteration, verifies the stopping condition, trigerring the
% corresponding FSMs, picking the best genes from parent and offsprings and cast them as the new generation
% to all of the FSMs.
handleIteration(State,Active_Nodes,Mutate_iteration) ->
  Track = State#state.track, Maps_list = tl(tuple_to_list(Track)),
  Maps = filterNodesMaps(Active_Nodes, Maps_list, []),
  Readys = [ready || M <-Maps, maps:find(Mutate_iteration,M)=/=error],

  case length(Readys)==length(Active_Nodes) of % Double check that we're fine
    true-> io:format("================================~n"),
      io:format("handleIteration:~p~n", [Mutate_iteration]),
      case Mutate_iteration==State#state.max_mutate_iteration of
        false->
          Updated_State = State#state{mutate_iteration = Mutate_iteration + 1},
          U_S = triggerCalcState(Mutate_iteration, Active_Nodes, Updated_State), U_S;
        true->
          io:format("Final Iteration:~n"),
          {Best_score,Best_Genotype,Processes_cnt} = chooseBest(Mutate_iteration,State),
          io:format("Best_score:~p~n", [Best_score]),
          io:format("Best_Genotype:~p~n", [Best_Genotype]),
          Cx = hd(Best_Genotype),
          Nurons_num = length(Cx#cortex.nids),
          Statistics = [{process, Processes_cnt},{neurons, Nurons_num},{fitness, Best_score}],
          if
            State#state.systemStopped==false ->
              Agent = utills:generateServerId(?MASTER_NODE, nn1),
              % Extract simulation path
              {ok,Path} = gen_server:call(Agent,{run_simulation,Best_Genotype}),
              io:format("hunter path:~p~n", [Path]),
              display(Path,Statistics),
              % Terminate the FSM, the FSM will terminate the supervisor and it's children's
              gen_statem:stop(State#state.pop_fsm_pid),
              State#state{systemStopped=true, simulationPath = Path};

            true ->
              display(State#state.simulationPath, Statistics),  State
          end
      end;
    false-> State
  end.
%%%-------------------------------------------------------------------

% Triggers the population FSM's, to acheive the next offspring generation.
triggerCalcState(Mutation_iterations,Active_Nodes,Updated_State)-> %%mnesia:force_load_table(db),
  {atomic,List} = database:read_all_mutateIter(Mutation_iterations),
  OffspringGenes = [{Score,{NNid,MutatIter}} || {db,NNid,MutatIter,_,_,Score} <-List],
  Process_List =  [Process|| {db,_,_,_,Process,_} <-List],
  Gene_List =  [Gene|| {db,_,_,Gene,_,_} <-List],
  Sortd_by_score = lists:keysort(1, OffspringGenes ++ Updated_State#state.parentGenes),
  BestGenotypes = lists:sublist(Sortd_by_score, Updated_State#state.nnPerNode),
  U_S = Updated_State#state{parentGenes = BestGenotypes},
  {BestScore,{BestNNid,_}} = hd(BestGenotypes),
  Short_Active_Nodes = [tl(hd(string:split(erl_types:atom_to_string(Node),"@")))||Node<-Active_Nodes],
  Statistics = [{process, lists:sum(Process_List)*length(Active_Nodes)},{neurons, neurons_amount(Gene_List,0)},
    {bestGeneScore, BestScore}, {gene_died, length(Sortd_by_score)-length(BestGenotypes)},{genration,Mutation_iterations},
    {active,Short_Active_Nodes}, {workload,length(OffspringGenes)/length(Active_Nodes)},{bestGeneID,BestNNid}],

  io:format("Best iteration score:~p~n", [BestScore]),
  io:format("Best iteration NN_ID:~p~n", [BestNNid]),
  io:format("Cuurent active nodes:~p~n", [Short_Active_Nodes]),

  graphic:update_stat(Statistics),
  broadcastGenes(BestGenotypes,Active_Nodes,U_S), U_S.
%%%-------------------------------------------------------------------

% Cast to all the populations addresses a "trigger" request
broadcastGenes(BestGenotypes,Active_Nodes,Updated_State)->
  ChosenGenes = [{NNid,MutatIter}||{_,{NNid,MutatIter}} <-BestGenotypes],
  PopAddresses = [utills:generateServerId(Node, population_fsm)||Node<-Active_Nodes],
  TriggerCalc = fun(PopAddr)-> gen_server:cast({global, PopAddr}, {runNetwork, ChosenGenes, Updated_State#state.mutate_iteration}) end,
  lists:foreach(TriggerCalc, PopAddresses).
%%%-------------------------------------------------------------------

% Choosing the best genes for the next generation via selecting from the Parent genes
% and the newly offsprings
chooseBest(Mutation_iterations,State)->%%mnesia:force_load_table(db),
  {atomic,List} = database:read_all_mutateIter(Mutation_iterations),
  Filtered = [{Score,{NNid,MutatIter}}||{db,NNid,MutatIter,_,_,Score} <-List],
  Sortd_by_score = lists:keysort(1,Filtered++State#state.parentGenes),
  {_,{NNid,MutatIter}}=hd(Sortd_by_score),
  {atomic,Best_Net} = database:get({NNid,MutatIter}),
  {db,_,_,Genotype,Processes_cnt,Score_db}=hd(Best_Net),
  {Score_db,Genotype,Processes_cnt}.
%%%-------------------------------------------------------------------
%%% Utilities
%%%-------------------------------------------------------------------

% Gui display, update the Rabbit/Hunter locations and statistics.
display([],_)-> turnOffTimer;
display([[R_X,R_Y,H_X,H_Y]|Path],Statistics)->
  graphic:update_location({R_X*7,R_Y*7},{H_X*7,H_Y*7}),
  timer:sleep(130),
  display(Path,Statistics).

% Generate the seed Genes
generateSeeds(NN_amount,Layers)-> % Initialize State
  % Each node create it's own seed
  {NNids, AgentsIds} = Env_Params = utills:generateNNIds(1,NN_amount),
  AgentIdsZipped = lists:zip(NNids, AgentsIds),
  Seeds=[#db{nn_id = NNid,mutId =0,
    gene=genotype_gen:construct_Genotype(AgentId,rng,pts,Layers),
    processes_count = 0,score = 0}||{NNid, AgentId}<-AgentIdsZipped],
  database:write_records(Seeds), Env_Params.

% Node monitoring Utilities
findActiveNodes()->
  NodeList = [?MASTER_NODE, ?NODE1, ?NODE2, ?NODE3],
  [Node || Node<- NodeList, net_adm:ping(Node)==pong].

findSlaves()->
  NodeList = [?NODE1, ?NODE2, ?NODE3],
  [Node || Node<- NodeList, net_adm:ping(Node)==pong].

% Make sure all the nodes have started their Mnesia service.
collectMnesiaStartMsgs([])-> ok;
collectMnesiaStartMsgs([S|Slaves])->
  receive
    {S, mnesiaStarted} -> collectMnesiaStartMsgs(Slaves)
  end.
% extract the neurons amount to display in the gui.
neurons_amount([],Acc)->Acc;
neurons_amount([Gene|Genes],Acc)-> Curr=hd(Gene),neurons_amount(Genes,Acc+length(Curr#cortex.nids)).

filterNodesMaps([],_, Acc)-> lists:reverse(Acc);

filterNodesMaps([Node|ActiveNodes],Maps, Acc)->
  {_, Map} = lists:keyfind(Node, 1, Maps),
  filterNodesMaps(ActiveNodes, Maps, [Map|Acc]).
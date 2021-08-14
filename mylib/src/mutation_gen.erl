%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% The module performs a random mutation several times to mutate a current gene
%%% The mutate function receives a gene and return's it's offspring.
%%% @end
%%% Created : 31. Jul 2021 5:16 PM
%%%-------------------------------------------------------------------
-module(mutation_gen).
-author("tom").
-include("records.hrl").
-define(AF_list, [tanh,cos,sin,sigmoid]).

%% API
-export([mutate/1]).

% Function which randomly choose a number of iteration between 1 TO sqrt(NeuronsAmount)
mutate(G)->
  Cx = hd(G),
  Nids = Cx#cortex.nids,
  NumOfNeurons=length(Nids),
  NUM=round(math:pow(NumOfNeurons,1/2)),
  N=rand:uniform(NUM),
  % Creates an ETS for internal use
  T = ets:new(genotype,[bag,private]),
  [ets:insert(T,E)||E<-G],
  mutate(T,N,Cx).

mutate(T,0,_)-> [Head|Tail] = ets:tab2list(T),L = lists:reverse(Tail),
  New_Genotype = [hd(L)]++[Head]++tl(L),New_Genotype;

% randomly select a mutation operation
mutate(T,N,Cx)->
  X = rand:uniform(8),
  case X of
    1->
      setBias(T,N,Cx);
    2->
      removeBias(T,N,Cx);
    3->
      addEdge(T,N,Cx);
    4->
      addNeuron(T,N,Cx);
    5->
      removeEdge(T,N,Cx);
    6->
      changeWeight(T,N,Cx);
    7->
      removeNeuron(T,N,Cx);
    8->
      changeAF(T,N,Cx)
  end.
%%%-------------------------------------------------------------------

% Sets a new bias to a randomly choose neuron
setBias(T,N,Cx)->
  Nids = Cx#cortex.nids,
  I=rand:uniform(length(Nids)),
  Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Input_idps = Neuron#neuron.input_idps,
  Updated_InputIdPs = lists:sublist(Input_idps,length(Input_idps)-1)++[{bias,rand:uniform()-0.5}],
  U_N = Neuron#neuron{input_idps = Updated_InputIdPs},
  ets:delete_object(T,Neuron),
  ets:insert(T,U_N),
  mutate(T,N-1,Cx).

% deletes a bias to a randomly choose neuron
removeBias(T,N,Cx)->
  Nids = Cx#cortex.nids,
  I=rand:uniform(length(Nids)),
  Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Input_idps = Neuron#neuron.input_idps,
  Updated_InputIdPs = lists:sublist(Input_idps,length(Input_idps)-1)++[{bias,0}],
  U_N = Neuron#neuron{input_idps = Updated_InputIdPs},
  ets:delete_object(T,Neuron),
  ets:insert(T,U_N),
  mutate(T,N-1,Cx).
%%%-------------------------------------------------------------------
% Adds an edge between randomly picked 2 neurons, the edge direction is forward only to prevent loops
addEdge(T,N,Cx)->
  From_list = Cx#cortex.sensor_ids++Cx#cortex.nids,
  {Name_from,Id_from} = lists:nth(rand:uniform(length(From_list)), From_list),
  To_list = case Name_from of
              neuron->{Layer,_} = Id_from, [{Name,{L,Id}}||{Name,{L,Id}} <-Cx#cortex.nids,L > Layer];
              sensor-> Cx#cortex.nids
            end,
  case To_list of
    []-> mutate(T,N,Cx);
    _-> {Name_to,Id_to} = lists:nth(rand:uniform(length(To_list)), To_list),
      Tab = addEdge(T,Name_from,Id_from,Name_to,Id_to,from),
      case Tab =:= exist of
        true -> mutate(T,N,Cx);
        false-> New_T = addEdge(Tab,Name_from,Id_from,Name_to,Id_to,to),mutate(New_T,N-1,Cx)
      end
  end.


addEdge(T,neuron,Id_from,Name_to,Id_to,from)->
  From_table = hd(ets:select(T, [{#neuron{id={neuron,Id_from}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Fanout = From_table#neuron.output_ids,
  case lists:member({Name_to,Id_to},Fanout) of
    true -> exist;
    false-> U_N = From_table#neuron{output_ids = Fanout++[{Name_to,Id_to}]},
      ets:delete_object(T,From_table), ets:insert(T,U_N),T
  end;


addEdge(T,sensor,Id_from,Name_to,Id_to,from)->
  From_table = hd(ets:select(T, [{#sensor{id={sensor,Id_from}, cx_id='_', name='_', vl = '_',fanout_ids='_'}, [], ['$_']}])),
  Fanout = From_table#sensor.fanout_ids,
  case lists:member({Name_to,Id_to},Fanout) of
    true -> exist;
    false-> U_S = From_table#sensor{fanout_ids = Fanout++[{Name_to,Id_to}]},
      ets:delete_object(T,From_table), ets:insert(T,U_S),T
  end;


addEdge(Tab,Name_from,Id_from,neuron,Id_to,to)->
  To_table = hd(ets:select(Tab,[{#neuron{id={neuron,Id_to}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Old = To_table#neuron.input_idps,
  case Name_from of
    sensor ->New = [{{Name_from,Id_from},[rand:uniform()-0.5,rand:uniform()-0.5,rand:uniform()-0.5,rand:uniform()-0.5]}]++Old;
    neuron-> New = [{{Name_from,Id_from},[rand:uniform()-0.5]}]++Old
  end,
  U_N = To_table#neuron{input_idps=New},
  ets:delete_object(Tab,To_table), ets:insert(Tab,U_N), Tab.

%%%-------------------------------------------------------------------
% removes an edge between randomly picked 2 neurons, the edge direction is forward only to prevent loops

removeEdge(T,N,Cx)->
  Final_layer = lists:max([L||{_,{L,_}} <-Cx#cortex.nids]),
  From_list = [{Name,{L,Id}}||{Name,{L,Id}} <-Cx#cortex.nids,L<Final_layer],
  {Name_from,Id_from} = lists:nth(rand:uniform(length(From_list)), From_list),
  {Layer,_} = Id_from, To_list = [{Name,{L,Id}}||{Name,{L,Id}} <-Cx#cortex.nids,L >= Layer],
  {Name_to,Id_to} = lists:nth(rand:uniform(length(To_list)), To_list),
  case legal_edge(T,Id_to) of
    true->
      Tab = removeEdge(T,Name_from,Id_from,Name_to,Id_to,from),
      case Tab =:= dont_exist of
        true -> mutate(T,N,Cx);
        false-> New_T = removeEdge(Tab,Name_from,Id_from,Name_to,Id_to,to), mutate(New_T,N-1,Cx)
      end;
    false-> mutate(T,N,Cx)
  end.


removeEdge(T,neuron,Id_from,Name_to,Id_to,from)->
  From_table = hd(ets:select(T, [{#neuron{id={neuron,Id_from}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Fanout = From_table#neuron.output_ids,
  case lists:member({Name_to,Id_to},Fanout) of
    true-> U_N = From_table#neuron{output_ids = lists:delete({Name_to,Id_to},Fanout)},
      ets:delete_object(T,From_table), ets:insert(T,U_N),T;
    false-> dont_exist
  end;


removeEdge(T,sensor,Id_from,Name_to,Id_to,from)->
  From_table = hd(ets:select(T, [{#sensor{id={sensor,Id_from}, cx_id='_', name='_', vl = '_',fanout_ids='_'}, [], ['$_']}])),
  Fanout = From_table#sensor.fanout_ids, case lists:member({Name_to,Id_to},Fanout) of
                                           true-> U_S = From_table#sensor{fanout_ids = lists:delete({Name_to,Id_to},Fanout)},
                                             ets:delete_object(T,From_table), ets:insert(T,U_S),T;
                                           false -> dont_exist
                                         end;


removeEdge(Tab,Name_from,Id_from,neuron,Id_to,to)->
  To_table = hd(ets:select(Tab,[{#neuron{id={neuron,Id_to}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Old = To_table#neuron.input_idps,
  New = lists:keydelete({Name_from,Id_from},1,Old),
  U_N = To_table#neuron{input_idps=New},
  ets:delete_object(Tab,To_table),
  ets:insert(Tab,U_N), Tab.

%%%-------------------------------------------------------------------
% Change a weight to a randomly picked neuron
changeWeight(T,N,Cx)->
  Nids = Cx#cortex.nids,
  I=rand:uniform(length(Nids)),
  Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Input_idps = Neuron#neuron.input_idps,
  case length(Input_idps)==1 of
    true->  mutate(T,N,Cx);
    false->I1=rand:uniform(length(Input_idps)-1),
      {Id,Weights} = lists:nth(I1, Input_idps),
      I2=rand:uniform(length(Weights)),
      N_W = lists:sublist(Weights,I2-1)++[rand:uniform()-0.5]++lists:nthtail(I2,Weights),
      N_Input_idps = lists:sublist(Input_idps,I1-1)++[{Id,N_W}]++lists:nthtail(I1,Input_idps),
      U_N = Neuron#neuron{input_idps = N_Input_idps},
      ets:delete_object(T,Neuron),
      ets:insert(T,U_N),
      mutate(T,N-1,Cx)
  end.

%%%-------------------------------------------------------------------
% add a neuron with randomly picked params
addNeuron(T,N,Cx)->
  Old = Cx#cortex.nids,
  Layer = rand:uniform(lists:max([L||{_,{L,_}} <- Old])-1),
  To_list = [{Name,{L,Id}}||{Name,{L,Id}} <-Old,L > Layer],
  {Name_to,Id_to} = lists:nth(rand:uniform(length(To_list)),To_list),
  From_list = [{Name,{L,Id}}||{Name,{L,Id}} <-Old,L < Layer]++Cx#cortex.sensor_ids,
  {Name_from,Id_from} = lists:nth(rand:uniform(length(From_list)),From_list),
  N_id = genotype_gen:generate_id(),
  Neuron = #neuron{id ={neuron,{Layer,N_id}},cx_id = Cx#cortex.id,af = tanh, input_idps=[{bias,rand:uniform()-0.5}],output_ids=[]},
  ets:insert(T,Neuron),
  New_Cx = Cx#cortex{nids = [Neuron#neuron.id]++Old},
  ets:delete_object(T,Cx),
  ets:insert(T,New_Cx),
  Tab = addEdge(T,Name_from,Id_from,neuron,{Layer,N_id},from),
  Tab1=addEdge(Tab,Name_from,Id_from,neuron,{Layer,N_id},to),
  Tab2 = addEdge(Tab1,neuron,{Layer,N_id},Name_to,Id_to,from),
  Tab3=addEdge(Tab2,neuron,{Layer,N_id},Name_to,Id_to,to),
  mutate(Tab3,N-1,New_Cx).
%%%-------------------------------------------------------------------

% remove a neuron, while removing the neuron's edges. connect it's input neurons to it's output neurons
% instead of leaving a hole in the network.
removeNeuron(T,N,Cx)->
  Layers = [L||{_,{L,_}} <-Cx#cortex.nids],
  Final_layer = lists:max(Layers),
  First_layer=lists:min(Layers),
  Middle_neurons=[{Name,{L,ID}}||{Name,{L,ID}} <-Cx#cortex.nids,First_layer<L,L<Final_layer],
  case Middle_neurons of
    []-> mutate(T,N,Cx);
    _->Id_chosen = lists:nth(rand:uniform(length(Middle_neurons)),Middle_neurons),
      Chosen_Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
      Inputs_tmp = [Id||{Id,_} <- Chosen_Neuron#neuron.input_idps],
      Inputs_ids = lists:sublist(Inputs_tmp,length(Inputs_tmp)-1),
      Inputs_Weight= lists:sublist( Chosen_Neuron#neuron.input_idps,length( Chosen_Neuron#neuron.input_idps)-1),
      Outputs=Chosen_Neuron#neuron.output_ids,
      Tab = change_output(T,Inputs_ids,Outputs,Id_chosen),
      New_Tab = change_input(Tab,Inputs_Weight,Outputs,Id_chosen),
      New_Cx = Cx#cortex{nids = lists:delete(Chosen_Neuron#neuron.id,Cx#cortex.nids)},
      ets:delete_object(New_Tab,Cx),
      ets:insert(New_Tab,New_Cx),
      ets:delete_object(New_Tab,Chosen_Neuron),
      mutate(Tab,N-1,New_Cx)
  end.
%%%-------------------------------------------------------------------

% Change the activation function of a certain gene
changeAF(T,N,Cx)->
  Nids = Cx#cortex.nids,
  I=rand:uniform(length(Nids)),
  Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  U_N = Neuron#neuron{af = lists:nth(rand:uniform(length(?AF_list)), ?AF_list)},
  ets:delete_object(T,Neuron),
  ets:insert(T,U_N),
  mutate(T,N-1,Cx).
%%%-------------------------------------------------------------------
%%% General utilities
%%%-------------------------------------------------------------------

change_input(T,_,[],_)->T;
change_input(T,Inputs,[{Name,Id}|Outputs],Id_chosen)->
  Chosen = hd(ets:select(T, [{#neuron{id={Name,Id}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Chosen_inputs = Chosen#neuron.input_idps,
  Chosen_inputs_U = [{I1,W}||{I1,W}<-Chosen_inputs,I1 =/= Id_chosen],
  Inputs_U = [{I2,W}||{I2,W}<-Inputs,lists:keymember(I2,1,Chosen_inputs_U)==false],
  New_inputs = Inputs_U++Chosen_inputs_U,
  U_Chosen = Chosen#neuron{input_idps = New_inputs},
  ets:delete_object(T,Chosen),
  ets:insert(T,U_Chosen),
  change_input(T,Inputs,Outputs,Id_chosen).

change_output(T,[],_,_)->T;
change_output(T,[{Name,Id}|Inputs],Outputs,Id_chosen)->
  case Name of
    neuron-> Chosen = hd(ets:select(T, [{#neuron{id={Name,Id}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
      Chosen_outputs = Chosen#neuron.output_ids,
      Chosen_outputs_U = lists:delete(Id_chosen,Chosen_outputs),
      Outputs_U = [Elem||Elem<-Outputs,lists:member(Elem,Chosen_outputs_U)==false],
      U_Chosen = Chosen#neuron{output_ids =  Chosen_outputs_U++Outputs_U},U_Chosen;
    sensor-> Chosen = hd(ets:select(T, [{#sensor{id={Name,Id}, cx_id='_', name='_', vl = '_',fanout_ids='_'}, [], ['$_']}])),
      Chosen_outputs = Chosen#sensor.fanout_ids,
      Chosen_outputs_U = lists:delete(Id_chosen,Chosen_outputs),
      Outputs_U = [Elem||Elem<-Outputs,lists:member(Elem,Chosen_outputs_U)==false],

      U_Chosen = Chosen#sensor{fanout_ids = Chosen_outputs_U++Outputs_U},U_Chosen
  end,
  ets:delete_object(T,Chosen),
  ets:insert(T,U_Chosen),
  change_output(T,Inputs,Outputs,Id_chosen).


legal_edge(Tab, Id_to)->
  To_table = hd(ets:select(Tab,[{#neuron{id={neuron,Id_to}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Old = To_table#neuron.input_idps,
  case length(Old)==2 of
    true-> false;
    false-> true
  end.




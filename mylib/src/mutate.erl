%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2021 5:16 PM
%%%-------------------------------------------------------------------
-module(mutate).
-author("tom").
-include("records.hrl").
%% API
-export([mutate/1]).
mutate(G)->
  Cx = hd(G),
  Nids = Cx#cortex.nids,
  NumOfNeurons=length(Nids),
  NUM=round(math:pow(NumOfNeurons,1/2)),
  N=rand:uniform(NUM),
  T = ets:new(genotype,[bag,private]),
  [ets:insert(T,E)||E<-G],
  mutate(T,N,Cx).
mutate(T,0,_)-> [Head|Tail] = ets:tab2list(T),L = lists:reverse(Tail),
  New_Genotype = [hd(L)]++[Head]++tl(L),New_Genotype;
mutate(T,N,Cx)->
  io:format("Iteration:~p~n",[N]),
  X=rand:uniform(6),
  case X of
    1->setBias(T,N,Cx);
    2->removeBias(T,N,Cx);
    3->addEdge(T,N,Cx);
    4->addNeuron(T,N,Cx);
    5->removeEdge(T,N,Cx);
    6->changeWeight(T,N,Cx)
  end.

setBias(T,N,Cx)-> Nids = Cx#cortex.nids, I=rand:uniform(length(Nids)), Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Input_idps = Neuron#neuron.input_idps,
  Updated_InputIdPs = lists:sublist(Input_idps,length(Input_idps)-1)++[{bias,rand:uniform()-0.5}],
  U_N = Neuron#neuron{input_idps = Updated_InputIdPs}, ets:delete_object(T,Neuron), ets:insert(T,U_N),mutate(T,N-1,Cx).

removeBias(T,N,Cx)-> Nids = Cx#cortex.nids, I=rand:uniform(length(Nids)), Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Input_idps = Neuron#neuron.input_idps,
  Updated_InputIdPs = lists:sublist(Input_idps,length(Input_idps)-1)++[{bias,0}],
  U_N = Neuron#neuron{input_idps = Updated_InputIdPs}, ets:delete_object(T,Neuron), ets:insert(T,U_N),mutate(T,N-1,Cx).

addEdge(T,N,Cx)-> From_list = Cx#cortex.sensor_ids++Cx#cortex.nids,
  {Name_from,Id_from} = lists:nth(rand:uniform(length(From_list)), From_list),
  case Name_from of
    neuron->{Layer,_} = Id_from, To_list = [{Name,{L,Id}}||{Name,{L,Id}} <-Cx#cortex.nids,L >= Layer]++Cx#cortex.actuator_ids;
    sensor-> To_list = Cx#cortex.nids++Cx#cortex.actuator_ids
  end,
  {Name_to,Id_to} = lists:nth(rand:uniform(length(To_list)), To_list),
  Tab = addEdge(T,Name_from,Id_from,Name_to,Id_to,from),case Tab =:= exist of
                                                          true -> mutate(T,N-1,Cx);
                                                          false-> New_T = addEdge(Tab,Name_from,Id_from,Name_to,Id_to,to),mutate(New_T,N-1,Cx)
                                                        end.

addEdge(T,neuron,Id_from,Name_to,Id_to,from)->From_table = hd(ets:select(T, [{#neuron{id={neuron,Id_from}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Fanout = From_table#neuron.output_ids, case lists:member({Name_to,Id_to},Fanout) of
                                           true -> exist;
                                           false-> U_N = From_table#neuron{output_ids = Fanout++[{Name_to,Id_to}]},
                                             ets:delete_object(T,From_table), ets:insert(T,U_N),T
                                           end;

addEdge(T,sensor,Id_from,Name_to,Id_to,from)-> From_table = hd(ets:select(T, [{#sensor{id={sensor,Id_from}, cx_id='_', name='_', vl = '_',fanout_ids='_'}, [], ['$_']}])),
  Fanout = From_table#sensor.fanout_ids, case lists:member({Name_to,Id_to},Fanout) of
                                           true -> exist;
                                           false-> U_S = From_table#sensor{fanout_ids = Fanout++[{Name_to,Id_to}]},
                                             ets:delete_object(T,From_table), ets:insert(T,U_S),T
                                         end;
addEdge(Tab,Name_from,Id_from,neuron,Id_to,to)->
  To_table = hd(ets:select(Tab,[{#neuron{id={neuron,Id_to}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Old = To_table#neuron.input_idps,New = [{{Name_from,Id_from},[rand:uniform()-0.5]}]++Old,U_N = To_table#neuron{input_idps=New},
  ets:delete_object(Tab,To_table), ets:insert(Tab,U_N), Tab;

addEdge(Tab,Name_from,Id_from,actuator,Id_to,to)->
  To_table = hd(ets:select(Tab, [{#actuator{id={actuator,Id_to}, cx_id='_', name='_', vl = '_',fanin_ids='_'}, [], ['$_']}])),
  Old = To_table#actuator.fanin_ids,New = [{Name_from,Id_from}]++Old,U_A = To_table#actuator{fanin_ids=New},
  ets:delete_object(Tab,To_table), ets:insert(Tab,U_A), Tab.

removeEdge(T,N,Cx)-> From_list = Cx#cortex.sensor_ids++Cx#cortex.nids,
  {Name_from,Id_from} = lists:nth(rand:uniform(length(From_list)), From_list),
  case Name_from of
    neuron->{Layer,_} = Id_from, To_list = [{Name,{L,Id}}||{Name,{L,Id}} <-Cx#cortex.nids,L >= Layer]++Cx#cortex.actuator_ids;
    sensor-> To_list = Cx#cortex.nids++Cx#cortex.actuator_ids
  end,
  {Name_to,Id_to} = lists:nth(rand:uniform(length(To_list)), To_list),Tab = removeEdge(T,Name_from,Id_from,Name_to,Id_to,from),
  case Tab =:= dont_exist of
    true -> mutate(T,N-1,Cx);
    false-> New_T = removeEdge(Tab,Name_from,Id_from,Name_to,Id_to,to), mutate(New_T,N-1,Cx)
  end.


removeEdge(T,neuron,Id_from,Name_to,Id_to,from)->From_table = hd(ets:select(T, [{#neuron{id={neuron,Id_from}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Fanout = From_table#neuron.output_ids, case lists:member({Name_to,Id_to},Fanout) of
                                           true-> U_N = From_table#neuron{output_ids = lists:delete({Name_to,Id_to},Fanout)},
                                             ets:delete_object(T,From_table), ets:insert(T,U_N),T;
                                           false-> dont_exist
                                         end;

removeEdge(T,sensor,Id_from,Name_to,Id_to,from)-> From_table = hd(ets:select(T, [{#sensor{id={sensor,Id_from}, cx_id='_', name='_', vl = '_',fanout_ids='_'}, [], ['$_']}])),
  Fanout = From_table#sensor.fanout_ids, case lists:member({Name_to,Id_to},Fanout) of
                                           true-> U_S = From_table#sensor{fanout_ids = lists:delete({Name_to,Id_to},Fanout)},
                                             ets:delete_object(T,From_table), ets:insert(T,U_S),T;
                                           false -> dont_exist
                                         end;
removeEdge(Tab,Name_from,Id_from,neuron,Id_to,to)->
  To_table = hd(ets:select(Tab,[{#neuron{id={neuron,Id_to}, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Old = To_table#neuron.input_idps,New = lists:keydelete({Name_from,Id_from},1,Old),U_N = To_table#neuron{input_idps=New},
  ets:delete_object(Tab,To_table), ets:insert(Tab,U_N), Tab;

removeEdge(Tab,Name_from,Id_from,actuator,Id_to,to)->
  To_table = hd(ets:select(Tab, [{#actuator{id={actuator,Id_to}, cx_id='_', name='_', vl = '_',fanin_ids='_'}, [], ['$_']}])),
  Old = To_table#actuator.fanin_ids,New = lists:delete({Name_from,Id_from},Old),U_A = To_table#actuator{fanin_ids=New},
  ets:delete_object(Tab,To_table), ets:insert(Tab,U_A), Tab.

changeWeight(T,N,Cx)-> Nids = Cx#cortex.nids, I=rand:uniform(length(Nids)), Id_chosen = lists:nth(I, Nids),
  Neuron = hd(ets:select(T, [{#neuron{id=Id_chosen, cx_id='_', af='_', input_idps = '_',output_ids='_'}, [], ['$_']}])),
  Input_idps = Neuron#neuron.input_idps, I1=rand:uniform(length(Input_idps)-1), {Id,Weights} = lists:nth(I1, Input_idps),
  I2=rand:uniform(length(Weights)),  N_W = lists:sublist(Weights,I2-1)++[rand:uniform()-0.5]++lists:nthtail(I2,Weights),
  N_Input_idps = lists:sublist(Input_idps,I1-1)++[{Id,N_W}]++lists:nthtail(I1,Input_idps),
  U_N = Neuron#neuron{input_idps = N_Input_idps},ets:delete_object(T,Neuron), ets:insert(T,U_N),mutate(T,N-1,Cx).

addNeuron(T,N,Cx)-> Old = Cx#cortex.nids, Layer = rand:uniform(lists:max([L||{_,{L,_}} <- Old])) ,
  Neuron = #neuron{
  id ={neuron,{Layer,constructor:generate_id()}},
  cx_id = Cx#cortex.id,
  af = tanh,
  input_idps=[],
  output_ids=[]},
  ets:insert(T,Neuron),
  New_Cx = Cx#cortex{nids = Old++[Neuron#neuron.id]}, ets:delete_object(T,Cx), ets:insert(T,New_Cx),
  ets:insert(T,Neuron),mutate(T,N-1,New_Cx).


%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------

-module(phenotype_gen).
% Necessary for dynamic calling
-compile(export_all).

-include("records.hrl").
-include("config.hrl").


bringGeneToLife()-> bringGeneToLife(ffnn).
bringGeneToLife(FileName)->
	{ok,Genotype} = file:consult(FileName),
	spawn(phenotype_gen,bringGeneToLife,[FileName,Genotype]).

% FileName with NN_ID
bringGeneToLife(FileName, Genotype)->
	% Tracking pids to kill the network
	IdsNPIds = ets:new(idsNpids,[set,private]),
	[Cx|CerebralUnits] = Genotype,

	Sensor_Ids = Cx#cortex.sensor_ids,
	Actuator_Ids = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.nids,

	% Spawn all entities
	spawn_NN_ComponentsType(IdsNPIds,cortex,[Cx#cortex.id]),
	spawn_NN_ComponentsType(IdsNPIds,sensor,Sensor_Ids),
	spawn_NN_ComponentsType(IdsNPIds,actuator,Actuator_Ids),
	spawn_NN_ComponentsType(IdsNPIds,neuron,NIds),

	% Counting how many processes in this Mutation iteration
	ProcessesCount = length(Sensor_Ids ++ Actuator_Ids ++ NIds) + 1,

	% Initialize entities
	connectNN_Component(CerebralUnits,IdsNPIds),
	connectCortex(Cx,IdsNPIds),
	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),
	receive
		% Writing to a file in the end of the process
		{Cx_PId,score_and_backup,{Neuron_IdsNWeights, Score, SimStepsVec}} ->
			case ?TRACK_GENES of
				true-> writeLog(FileName, IdsNPIds,Genotype,Neuron_IdsNWeights);
				false-> dontTrackGenes
			end,

			{Score, ProcessesCount, SimStepsVec}

		% update the genotype score
	end.

spawn_NN_ComponentsType(IdsNPIds,CerebralUnitType,[Id|Ids])->

	PId = case CerebralUnitType of
					actuator-> InitLoc = ?HUNTER_INIT_LOC,
						CerebralUnitType:gen(self(),node(), InitLoc);
					_-> CerebralUnitType:gen(self(),node())
				end,
	% Insert pids and ids
	ets:insert(IdsNPIds,{Id,PId}),
	ets:insert(IdsNPIds,{PId,Id}),

	spawn_NN_ComponentsType(IdsNPIds,CerebralUnitType,Ids);
spawn_NN_ComponentsType(_IdsNPIds,_CerebralUnitType,[])-> true.


connectNN_Component([R|Records],IdsNPIds) when is_record(R,sensor) ->
	SId = R#sensor.id,
	SPId = ets:lookup_element(IdsNPIds,SId,2),
	Cx_PId = ets:lookup_element(IdsNPIds,R#sensor.cx_id,2),

	SName = R#sensor.name,
	Fanout_Ids = R#sensor.fanout_ids,
	Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],

	SensoryVector = generateRabbitPatrol(),
	% Init message
	SPId ! {self(),{SId,Cx_PId,SName,R#sensor.vl,Fanout_PIds,SensoryVector}},
	connectNN_Component(Records,IdsNPIds);

connectNN_Component([R|Records],IdsNPIds) when is_record(R,actuator) ->
	AId = R#actuator.id,
	APId = ets:lookup_element(IdsNPIds,AId,2),
	Cx_PId = ets:lookup_element(IdsNPIds,R#actuator.cx_id,2),
	AName = R#actuator.name,
	Fanin_Ids = R#actuator.fanin_ids,
	Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],
	APId ! {self(),{AId,Cx_PId,AName,Fanin_PIds}},
	connectNN_Component(Records,IdsNPIds);

connectNN_Component([R|Records],IdsNPIds) when is_record(R,neuron) ->
	NId = R#neuron.id,
	NPId = ets:lookup_element(IdsNPIds,NId,2),
	Cx_PId = ets:lookup_element(IdsNPIds,R#neuron.cx_id,2),
	AFName = R#neuron.af,
	Input_IdPs = R#neuron.input_idps,
	Output_Ids = R#neuron.output_ids,
	Input_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]),
	Output_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],
	NPId ! {self(),{NId,Cx_PId,AFName,Input_PIdPs,Output_PIds}},
	connectNN_Component(Records,IdsNPIds);

connectNN_Component([],_IdsNPIds)-> ok.

convert_IdPs2PIdPs(_IdsNPIds,[{bias,Bias}],Acc)->
	lists:reverse([Bias|Acc]);
convert_IdPs2PIdPs(IdsNPIds,[{Id,Weights}|Fanin_IdPs],Acc)->
	convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),Weights}|Acc]).

connectCortex(Cx,IdsNPIds) ->
	Cx_Id = Cx#cortex.id,
	Cx_PId = ets:lookup_element(IdsNPIds,Cx_Id,2),
	SIds = Cx#cortex.sensor_ids,
	AIds = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.nids,
	SPIds = [ets:lookup_element(IdsNPIds,SId,2) || SId <- SIds],
	APIds = [ets:lookup_element(IdsNPIds,AId,2) || AId <- AIds],
	NPIds = [ets:lookup_element(IdsNPIds,NId,2) || NId <- NIds],


	Cx_PId ! {self(),{Cx_Id,SPIds,APIds,NPIds}, ?SIM_ITERATIONS -2 }.


update_genotype(IdsNPIds,Genotype,[{N_Id,PIdPs}|WeightPs])->
	N = lists:keyfind(N_Id, 2, Genotype),
	Updated_InputIdPs = convert_PIdPs2IdPs(IdsNPIds,PIdPs,[]),
	U_N = N#neuron{input_idps = Updated_InputIdPs},
	U_Genotype = lists:keyreplace(N_Id, 2, Genotype, U_N),
	update_genotype(IdsNPIds,U_Genotype,WeightPs);
update_genotype(_IdsNPIds,Genotype,[])-> Genotype.

convert_PIdPs2IdPs(IdsNPIds,[{PId,Weights}|Input_PIdPs],Acc)->
	convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),Weights}|Acc]);
convert_PIdPs2IdPs(_IdsNPIds,[Bias],Acc)->
	lists:reverse([{bias,Bias}|Acc]).


generateRabbitPatrol()->
	[[I,I]||I<-lists:seq(1,?SIM_ITERATIONS)].

writeLog(FileName, IdsNPIds,Genotype,Neuron_IdsNWeights)->
	U_Genotype = update_genotype(IdsNPIds,Genotype,Neuron_IdsNWeights),
	{ok, File} = file:open(FileName, write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, U_Genotype),
	file:close(File).
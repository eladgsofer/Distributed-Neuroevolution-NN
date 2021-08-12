-module(genotype_gen).
-compile(export_all).
-include("records.hrl").
-include("config.hrl").

-define(SENSOR_INPUT, 4).
-define(ACTUATOR_OUTPUT, 2).

construct_Genotype(FileName,HiddenLayerDensities)->
	% Creating The data structures records
	Sensor = #sensor{id={sensor,generate_id()},name=rng,vl=?SENSOR_INPUT},
	Actuator = #actuator{id={actuator,generate_id()},name=pts,vl=?ACTUATOR_OUTPUT},
	Cx_Id = {cortex,generate_id()},

	Output_VL = Actuator#actuator.vl,
	LayerDensities = lists:append(HiddenLayerDensities,[Output_VL]),
	Neurons = create_NeuroLayers(Cx_Id, Sensor, Actuator,LayerDensities),

	% Neurons including all the layers
	[Input_Layer|_] = Neurons, 
	[Output_Layer|_] = lists:reverse(Neurons),

	% Extract Ids
	FL_NIds = [N#neuron.id || N <- Input_Layer], 
	LL_NIds = [N#neuron.id || N <-  Output_Layer],

	% Create Cortex, Acutator, Sensor
	NIds = [N#neuron.id || N <- lists:flatten(Neurons)],
	Sensor = Sensor#sensor{cx_id = Cx_Id, fanout_ids = FL_NIds},
	Actuator = Actuator#actuator{cx_id=Cx_Id,fanin_ids = LL_NIds},
	Cortex = create_Cortex(Cx_Id,[Sensor#sensor.id],[Actuator#actuator.id],NIds),

	% Genotype
	Genotype = lists:flatten([Cortex,Sensor,Actuator|Neurons]), Genotype.

	create_NeuroLayers(Cx_Id,Sensor,Actuator,LayerDensities) ->
		% Sensor template id - not full
		Input_IdPs = [{Sensor#sensor.id, Sensor#sensor.vl}],
		Tot_Layers = length(LayerDensities),

		[FL_Neurons|Next_LDs] = LayerDensities,
		% Generate Neuron Ids for the first Layer
		NIds = [{neuron,{1,Id}}|| Id <- generate_ids(FL_Neurons,[])],
		% Deliver the previous layer ids, and how many neurons are in the next layer
		create_NeuroLayers(Cx_Id,Actuator#actuator.id,1,Tot_Layers,Input_IdPs,NIds,Next_LDs,[]).

	create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex,Tot_Layers,Input_IdPs,NIds,[Next_LD|LDs],Acc) ->
		% Generate next layer Ids
		Output_NIds = [{neuron,{LayerIndex+1,Id}} || Id <- generate_ids(Next_LD,[])],
		% Create a layer the previous layer and connect it to the next layer
		Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_NIds,[]),

		Next_InputIdPs = [{NId,1}|| NId <- NIds],
		create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex+1,Tot_Layers,Next_InputIdPs,Output_NIds,LDs,[Layer_Neurons|Acc]);
	create_NeuroLayers(Cx_Id,Actuator_Id,Tot_Layers,Tot_Layers,Input_IdPs,NIds,[],Acc) -> 
		Output_Ids = [Actuator_Id], 
		Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[]), lists:reverse([Layer_Neurons|Acc]).

		% Create neurons one by one - since it is fully connected all the output is the same for every neuron
		create_NeuroLayer(Cx_Id,Input_IdPs,[Id|NIds],Output_Ids,Acc) ->
			Neuron = create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids), 
			create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[Neuron|Acc]); 
		create_NeuroLayer(_Cx_Id,_Input_IdPs,[],_Output_Ids,Acc) -> Acc.

		create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids)-> 
			Proper_InputIdPs = create_NeuralInput(Input_IdPs,[]),
			#neuron{id=Id,cx_id = Cx_Id,af=tanh,input_idps=Proper_InputIdPs,output_ids=Output_Ids}. 

			create_NeuralInput([{Input_Id,Input_VL}|Input_IdPs],Acc) ->
				% create weights for every neuron
				Weights = create_NeuralWeights(Input_VL,[]),
				create_NeuralInput(Input_IdPs,[{Input_Id,Weights}|Acc]);
			create_NeuralInput([],Acc)-> 
				lists:reverse([{bias,rand:uniform()-0.5}|Acc]).
			 
				create_NeuralWeights(0,Acc) -> Acc;
				create_NeuralWeights(Index,Acc) ->
					W = rand:uniform()-0.5,
					create_NeuralWeights(Index-1,[W|Acc]). 

			generate_ids(0,Acc) ->
				Acc;
			generate_ids(Index,Acc)->
				Id = generate_id(),
				generate_ids(Index-1,[Id|Acc]).

			generate_id() ->
				{MegaSeconds,Seconds,MicroSeconds} = now(),
				1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

	create_Cortex(Cx_Id,S_Ids,A_Ids,NIds) ->
		#cortex{id = Cx_Id, sensor_ids=S_Ids, actuator_ids=A_Ids, nids = NIds}.
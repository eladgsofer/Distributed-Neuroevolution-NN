-module(cortex).
-compile(export_all).
-include("records.hrl").
-include("config.hrl").

gen(PhenoTypePid,Node)-> spawn(Node,?MODULE,loop,[PhenoTypePid]).

loop(ExoSelf_PId) ->
	receive
		{ExoSelf_PId,{Id,SPIds,APIds,NPIds}, TotSteps} ->
			put(start_time,now()),
			Init_loc=?HUNTER_INIT_LOC,
			[RabbitFLoc|_] = phenotype_gen:generateRabbitPatrol(),
			FirstSimStep = RabbitFLoc ++ Init_loc,
			[SPId ! {self(),sync,Init_loc} || SPId <- SPIds],
			loop(Id,ExoSelf_PId,SPIds,{APIds,APIds},NPIds,TotSteps, {Init_loc, 0, [FirstSimStep]})
	end.

% Terminating the network when simulation is over - backing up the current status
loop(Id,ExoSelf_PId,SPIds,{_APIds,MAPIds},NPIds,0, {HunterLoc, DistanceAcc, SimulationStepsAcc}) ->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	Fitness_Score = math:sqrt(DistanceAcc), % Euclead Norm
	SimStepsVec = lists:reverse(SimulationStepsAcc),
	% backup the network in a file
	ExoSelf_PId ! {self(),score_and_backup, {Neuron_IdsNWeights, Fitness_Score, SimStepsVec}},
	% Terminating all the network
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- MAPIds],
	[PId ! {self(),termiante} || PId <- NPIds];

loop(Id,ExoSelf_PId,SPIds,{[APId|APIds],MAPIds},NPIds,Step, {_, DistanceAcc, SimulationStepsAcc}) ->
	receive
		{APId,sync,HunterLoc} ->
			{CurrDist, SimStep} = calcDistance(Step, HunterLoc),
			loop(Id,ExoSelf_PId,SPIds,{APIds,MAPIds},NPIds,Step,{HunterLoc, CurrDist+DistanceAcc, [SimStep|SimulationStepsAcc]});
		terminate ->
			[PId ! {self(),terminate} || PId <- SPIds],
			[PId ! {self(),terminate} || PId <- MAPIds],
			[PId ! {self(),termiante} || PId <- NPIds]
	end;
loop(Id,ExoSelf_PId,SPIds,{[],MAPIds},NPIds,Step, {Hunter_loc, DistanceAcc, SimulationStepsAcc})->
	[PId ! {self(),sync,Hunter_loc} || PId <- SPIds],
	loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,Step-1,{Hunter_loc, DistanceAcc, SimulationStepsAcc}).

get_backup([NPId|NPIds],Acc)->
	NPId ! {self(),get_backup},
	receive
		{NPId,NId,WeightTuples}-> get_backup(NPIds,[{NId,WeightTuples}|Acc])
	end;
get_backup([],Acc)-> Acc.

calcDistance(Step, HunterLoc)->
	Actual_Step = ?SIM_ITERATIONS - Step,
	% Calc The rabbit coordinates
	RabbitVec = lists:seq(1,?SIM_ITERATIONS),

	RabbitLoc = lists:nth(Actual_Step, RabbitVec),
	[R_X, R_Y, H_X, H_Y] = [RabbitLoc, RabbitLoc] ++ HunterLoc,
	
	Distance = math:pow(R_X-H_X,2) + math:pow(R_Y-H_Y,2),
	{Distance, [R_X, R_Y, H_X, H_Y]}.

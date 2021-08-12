%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

% Network entities
-record(sensor, {id, cx_id, name, vl, fanout_ids}).
-record(actuator,{id, cx_id,  name, vl, fanin_ids}).
-record(neuron, {id, cx_id, af, input_idps, output_ids}).
-record(cortex, {id, sensor_ids, actuator_ids, nids}).
-record(genotype, {nn_id, score, processes_info}).

% NN DB entry
-record(db, {nn_id, mutId, gene, processes_count, score}).
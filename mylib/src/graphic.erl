-module(graphic).
-author("Tom").

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

%% API
-export([start/0, update_location/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(main_PC_state, {frame, panel, sensor_img}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
  wx_object:start({local, ?SERVER}, ?MODULE, [], []).

update_location(Rabbit_pos,Hunter_pos,Statistics) ->
  wx_object:call(?SERVER, {update_img, Rabbit_pos,Hunter_pos,Statistics}).


%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%-spec(init(Args :: term()) ->
%  {ok, State :: #main_PC_state{}} | {ok, State :: #main_PC_state{}, timeout() | hibernate} |
%  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(gui_db,[set,public,named_table]),
  ets:new(stats,[set,public,named_table]),
  ets:insert(stats,[{process, 0},{neurons, 0},{fitness, 0}, {distance, 10}]),
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "Neuroevolution Neural Network", [{size,{1920, 1080}}]),
  Panel  = wxPanel:new(Frame),
  wxFrame:createStatusBar(Frame),
  wxFrame:show(Frame),
  BackGround = wxBitmap:new("background.bmp"),
  Rabbit = wxBitmap:new("rabbit.bmp"),
  Hunter = wxBitmap:new("hunter.bmp"),
  %ets:insert(gui_db,{{10,10},Rabbit}),
  %ets:insert(gui_db,{{30,30},Hunter}),
  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
    Paint = wxBufferedPaintDC:new(Panel),
    wxDC:drawBitmap(Paint,BackGround,{0,0}),
    Text = lists:flatten(io_lib:format("Processes: ~p, Neurons: ~p, Fitness: ~p,  Distance: ~p",
      [ets:lookup_element(stats, process	 , 2),
        ets:lookup_element(stats, neurons , 2),
        ets:lookup_element(stats, fitness , 2),
        ets:lookup_element(stats, distance	 , 2)])),
    % Updating the status bar with the game info
    wxFrame:setStatusText(Frame, Text),
    drawETS(Paint),
    wxBufferedPaintDC:destroy(Paint) end,

  % connect panel
  wxFrame:connect(Panel, paint, [{callback, CallBackPaint}]),

  % Self messaging timer
  timer:send_interval(80, self(), graphicUpdate),

  %sensor:start(sensor1), %ToDo: temp
  {Frame, #main_PC_state{frame = Frame, panel = Panel}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #main_PC_state{}) ->
  {reply, Reply :: term(), NewState :: #main_PC_state{}} |
  {reply, Reply :: term(), NewState :: #main_PC_state{}, timeout() | hibernate} |
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #main_PC_state{}} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_call({update_img, Rabbit_pos,Hunter_pos,Statistics}, _From, State = #main_PC_state{ panel = Panel}) ->
  ets:delete_all_objects(ets:whereis(gui_db)),
  RabbitBMP = wxBitmap:new("rabbit.bmp"),
  HunterBMP = wxBitmap:new("hunter.bmp"),
  ets:insert(gui_db,{Rabbit_pos,RabbitBMP}),ets:insert(gui_db,{Hunter_pos,HunterBMP}),
  {X_hunter,Y_hunter} = Hunter_pos, {X_rabbit,Y_rabbit} = Rabbit_pos,
  %%Statistics=[{process, #num},{neurons, $num},{fitness, $num}, {distance, #num}]
  ets:insert(stats,Statistics++[{distance,math:sqrt(math:pow((X_hunter-X_rabbit),2)+math:pow((Y_hunter-Y_rabbit),2))}]),
  {reply, ok, State};
handle_call(_Request, _From, State = #main_PC_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_PC_state{}) ->
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_cast({update_img, {Sensor_Pos,Sensor_State}}, State = #main_PC_state{ panel = Panel}) ->
  State_img = case Sensor_State of
    active -> "active_sensor.bmp";
    sending -> "sensor_sending_data.bmp";
    asleep -> "sensor_sleep.bmp"
  end,
  case ets:member(gui_db,Sensor_Pos) of
    true ->
      [{Sensor_Pos,SensorBMP_prev}] = ets:lookup(gui_db,Sensor_Pos),
      wxBitmap:destroy(SensorBMP_prev);
    false -> ok
  end,
  SensorBMP = wxBitmap:new(State_img),
  ets:insert(gui_db,{Sensor_Pos,SensorBMP}),
  {noreply, State};
handle_cast(_Request, State = #main_PC_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #main_PC_state{}) ->
  {noreply, NewState :: #main_PC_state{}} |
  {noreply, NewState :: #main_PC_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_PC_state{}}).
handle_info(_Info, State = #main_PC_state{}) ->
  wxWindow:refresh(State#main_PC_state.panel,[{eraseBackground,false}]),{noreply, State},
  {noreply, State}.

handle_event(#wx{event = #wxClose{}},State = #main_PC_state {frame = Frame}) -> % close window event
  io:format("Exiting\n"),
  wxWindow:destroy(Frame),
  wx:destroy(),
  {stop,normal,State};

handle_event(_Event,State) -> % when left click has been pressed, activate navigation function
  {noreply,State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #main_PC_state{}) -> term()).
terminate(_Reason, _State = #main_PC_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #main_PC_state{},
    Extra :: term()) ->
  {ok, NewState :: #main_PC_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #main_PC_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% ===== iterates all ets elements and update image =====
drawETS(Painter) ->
  First = ets:first(gui_db),
  case First of
    '$end_of_table'-> ok;
    _ ->
      [{Pos,BMP}] = ets:lookup(gui_db,First),
      wxDC:drawBitmap(Painter, BMP, Pos),
      drawETS(Painter,First)
  end.
drawETS(Painter,Curr) ->
  Next = ets:next(gui_db,Curr),
  case Next of
    '$end_of_table' -> ok;
    _ ->
      [{Pos,BMP}] = ets:lookup(gui_db,Next),
      wxDC:drawBitmap(Painter, BMP, Pos),
      drawETS(Painter,Next)
  end.

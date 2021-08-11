-module(graphic).
-author("Tom").

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

%% API
-export([start/0, update_location/2,update_stat/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {frame, panel}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start() ->
  wx_object:start({local, ?SERVER}, ?MODULE, [], []).

update_location(Rabbit_pos,Hunter_pos) ->
  wx_object:call(?SERVER, {update_img, Rabbit_pos,Hunter_pos}).
update_stat(Statistics) ->
  wx_object:call(?SERVER, {statistics,Statistics}).


%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%-spec(init(Args :: term()) ->
%  {ok, State :: #main_PC_state{}} | {ok, State :: #main_PC_state{}, timeout() | hibernate} |
%  {stop, Reason :: term()} | ignore).
init([]) ->
  Width = 1920,
  Height = 1080,
  ets:new(gui_db,[set,public,named_table]),
  ets:new(stats,[set,public,named_table]),
  ets:insert(stats,{mode,learning}),
  ets:insert(stats,[{process, 0},{neurons, 0},{fitness, 0}, {distance, 0}]),
  W = wx:new(),
  Frame = wxFrame:new(W, -1, "result",[{size, {Width, Height}}]),
  Panel = wxPanel:new(Frame,[{size, {Width, Height}}]),
  Panel2 = wxPanel:new(Frame,[{size, {Width, Height}}]),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Font = wxFont:new(15, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_BOLD),

  Neurons_label = wxStaticText:new(Panel, ?wxID_ANY, "Neurons:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Neurons_label,100),
  wxTextCtrl:setFont(Neurons_label, Font),
  NeuroTXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(NeuroTXT, Font),
  wxTextCtrl:setEditable(NeuroTXT, false),

  Processes_Label = wxStaticText:new(Panel, ?wxID_ANY, "Processes:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Processes_Label,100),
  wxTextCtrl:setFont(Processes_Label, Font),
  ProcessesTXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(ProcessesTXT, Font),
  wxTextCtrl:setEditable(ProcessesTXT, false),

  Fit_Label = wxStaticText:new(Panel, ?wxID_ANY, "fitness:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Fit_Label,100),
  wxTextCtrl:setFont(Fit_Label, Font),
  FitTXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(FitTXT, Font),
  wxTextCtrl:setEditable(FitTXT, false),

  Gen_Label = wxStaticText:new(Panel, ?wxID_ANY, "Gen:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Gen_Label,100),
  GenTXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(Gen_Label, Font),
  wxTextCtrl:setFont(GenTXT, Font),
  wxTextCtrl:setEditable(GenTXT, false),

  Distance_label = wxStaticText:new(Panel, ?wxID_ANY, "Distance:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Distance_label,100),
  DisTXT = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "***************"}, {style, ?
  wxTE_RIGHT}]),
  wxTextCtrl:setFont(Distance_label, Font),
  wxTextCtrl:setFont(DisTXT, Font),
  wxTextCtrl:setEditable(DisTXT, false),

  CounterSizer = wxBoxSizer:new(?wxHORIZONTAL),
  CounterSizer2 = wxBoxSizer:new(?wxHORIZONTAL),

  wxSizer:add(CounterSizer, Fit_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(CounterSizer, FitTXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(CounterSizer, Gen_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(CounterSizer, GenTXT,     [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(CounterSizer, Processes_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(CounterSizer, ProcessesTXT,     [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(CounterSizer2, Neurons_label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(CounterSizer2, NeuroTXT, [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(CounterSizer2, Distance_label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(CounterSizer2, DisTXT, [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  wxSizer:add(MainSizer, CounterSizer, [{flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer, CounterSizer2, [{flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer,Panel2, [{flag, ?wxEXPAND}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),
  wxWindow:setMaxSize(Frame, wxWindow:getSize(Frame)),
  Vbox = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Vbox, Panel, [{flag, ?wxEXPAND}]),

  State = #state{frame = Frame, panel = Panel},


  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
    BackGround = wxBitmap:new("images/learning.bmp"),
    BackGround2 = wxBitmap:new("images/background.bmp"),
    case ets:lookup_element(stats, mode, 2)==finish of
      true->
        Frame1 = wxFrame:new(W, ?wxID_ANY, "Neuroevolution Neural Network", [{size,{1920, 1080}}]),
        Panel1  = wxPanel:new(Frame1),

%%        wxTextCtrl:destroy(ProcessesTXT),
%%        wxTextCtrl:destroy(NeuroTXT),
%%        wxTextCtrl:destroy(FitTXT),
%%        wxTextCtrl:destroy(GenTXT),
%%        wxTextCtrl:destroy(DisTXT),
%%        wxStaticText:destroy(Neurons_label),
%%        wxStaticText:destroy(Processes_Label),
%%        wxStaticText:destroy(Fit_Label),
%%        wxStaticText:destroy(Gen_Label),
%%        wxStaticText:destroy(Distance_label),
        Paint1 = wxBufferedPaintDC:new(Panel1),
        wxDC:drawBitmap(Paint1,BackGround2,{0,0}),
        drawETS(Paint1),
        wxBufferedPaintDC:destroy(Paint1);
      false-> wxTextCtrl:changeValue(ProcessesTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, process	 , 2)]))),
        wxTextCtrl:changeValue(FitTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, fitness , 2)]))),
        wxTextCtrl:changeValue(NeuroTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, neurons , 2)]))),
        wxTextCtrl:changeValue(GenTXT, lists:flatten(io_lib:format("~p", [7]))),      %TODO add generation
        wxTextCtrl:changeValue(DisTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, distance	 , 2)]))),
        wxPanel:refresh(ProcessesTXT),
        wxPanel:refresh(FitTXT),
        wxPanel:refresh(NeuroTXT),
        wxPanel:refresh(GenTXT),
        wxPanel:refresh(DisTXT),
        wxPanel:refresh(Frame), %refresh the panel

        Paint = wxBufferedPaintDC:new(Panel),
        wxDC:drawBitmap(Paint,BackGround,{0,0}),

        wxBufferedPaintDC:destroy(Paint)
    end,
    wxBitmap:destroy(BackGround),
    wxBitmap:destroy(BackGround2) end,


  wxFrame:connect(Panel, paint, [{callback, CallBackPaint}]),
  timer:send_interval(80, self(), graphicUpdate),

  wxFrame:show(Frame),
  {Frame,State}.




%======================================================================================================================
%%  ets:new(gui_db,[set,public,named_table]),
%%  ets:new(stats,[set,public,named_table]),
%%  ets:insert(stats,[{process, 0},{neurons, 0},{fitness, 0}, {distance, 0}]),
%%  WxServer = wx:new(),
%%  Frame = wxFrame:new(WxServer, ?wxID_ANY, "Neuroevolution Neural Network", [{size,{1920, 1080}}]),
%%  Panel  = wxPanel:new(Frame),
%%  wxFrame:createStatusBar(Frame),
%%  wxFrame:show(Frame),
%%  BackGround = wxBitmap:new("background.bmp"),
%%  %Rabbit = wxBitmap:new("rabbit.bmp"),
%%  %Hunter = wxBitmap:new("hunter.bmp"),
%%  %ets:insert(gui_db,{{10,10},Rabbit}),
%%  %ets:insert(gui_db,{{30,30},Hunter}),
%%  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
%%    Paint = wxBufferedPaintDC:new(Panel),
%%    wxDC:drawBitmap(Paint,BackGround,{0,0}),
%%    Text = lists:flatten(io_lib:format("Processes: ~p, Neurons: ~p, Fitness: ~p,  Distance: ~p",
%%      [ets:lookup_element(stats, process	 , 2),
%%        ets:lookup_element(stats, neurons , 2),
%%        ets:lookup_element(stats, fitness , 2),
%%        ets:lookup_element(stats, distance	 , 2)])),
%%    % Updating the status bar with the game info
%%    wxFrame:setStatusText(Frame, Text),
%%    drawETS(Paint),
%%    wxBufferedPaintDC:destroy(Paint) end,
%%
%%  % connect panel
%%  wxFrame:connect(Panel, paint, [{callback, CallBackPaint}]),
%%
%%  % Self messaging timer
%%  timer:send_interval(80, self(), graphicUpdate),
%%
%%  %sensor:start(sensor1), %ToDo: temp
%%  {Frame, #main_PC_state{frame = Frame, panel = Panel}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({update_img, Rabbit_pos,Hunter_pos}, _From, State = #state{ panel = Panel}) ->
  ets:insert(stats,{mode,finish}),
  ets:delete_all_objects(ets:whereis(gui_db)),
  RabbitBMP = wxBitmap:new("images/rabbit.bmp"),
  HunterBMP = wxBitmap:new("images/hunter.bmp"),
  ets:insert(gui_db,{Rabbit_pos,RabbitBMP}),ets:insert(gui_db,{Hunter_pos,HunterBMP}),
  %{X_hunter,Y_hunter} = Hunter_pos, {X_rabbit,Y_rabbit} = Rabbit_pos,
  %%Statistics=[{process, #num},{neurons, $num},{fitness, $num}, {distance, #num}]
  %ets:insert(stats,Statistics++[{distance,math:sqrt(math:pow((X_hunter-X_rabbit),2)+math:pow((Y_hunter-Y_rabbit),2))}]),
  {reply, ok, State};



handle_call({statistics,Statistics}, _From, State = #state{}) ->
  ets:insert(stats,Statistics),
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
  wxWindow:refresh(State#state.panel,[{eraseBackground,false}]),{noreply, State},
  {noreply, State}.

handle_event(#wx{event = #wxClose{}},State = #state{frame = Frame}) -> % close window event
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
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
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

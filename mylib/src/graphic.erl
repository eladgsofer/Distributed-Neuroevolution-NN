%%%-------------------------------------------------------------------
%%% @author Tom
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------

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

init([]) ->
  Width = 1850,
  Height = 1080,
  ets:new(gui_db,[set,public,named_table]),
  ets:new(stats,[set,public,named_table]),
  ets:insert(stats,{mode,learning}),
  ets:insert(stats,{position,1}),
  ets:insert(stats,[{process, 0},{neurons, 0},{bestGeneScore, 0}, {gene_died, 0},
    {genration,0},{active,king},{workload,0},{bestGeneID,none}]),

  %S = [{process, 55},{neurons, 55},{bestGeneScore, 55}, {gene_died, 55},{genration,55},{active,tom},{workload,55},{bestGeneID,tom}]
  W = wx:new(),
  MainFrame = wxFrame:new(W, -1, "Learning process", [{size, {Width, Height}}]),
  ets:insert(stats,{firstSimStarted,true}),

  %add active nodes
  MainPanel = wxPanel:new(MainFrame,[{size, {Width, Height}}]),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Font = wxFont:new(15, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_BOLD),

  %%%===================================================================
  %%% Define all the labels/text boxes and other WX objects

  Neurons_label = wxStaticText:new(MainPanel, ?wxID_ANY, "Total Neurons:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Neurons_label,100),
  wxTextCtrl:setFont(Neurons_label, Font),
  NeuroTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(NeuroTXT, Font),
  wxTextCtrl:setEditable(NeuroTXT, false),

  ActiveNodes_label = wxStaticText:new(MainPanel, ?wxID_ANY, "Active Nodes:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(ActiveNodes_label,100),
  wxTextCtrl:setFont(ActiveNodes_label, Font),
  ActiveNodesTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(ActiveNodesTXT, Font),
  wxTextCtrl:setEditable(ActiveNodesTXT, false),

  Work_label = wxStaticText:new(MainPanel, ?wxID_ANY, "Node's workload:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Work_label,100),
  wxTextCtrl:setFont(Work_label, Font),
  WorkTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(WorkTXT, Font),
  wxTextCtrl:setEditable(WorkTXT, false),

  Processes_Label = wxStaticText:new(MainPanel, ?wxID_ANY, "Total Processes:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Processes_Label,100),
  wxTextCtrl:setFont(Processes_Label, Font),
  ProcessesTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(ProcessesTXT, Font),
  wxTextCtrl:setEditable(ProcessesTXT, false),

  GeneId_Label = wxStaticText:new(MainPanel, ?wxID_ANY, "Best gen ID:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(GeneId_Label,100),
  wxTextCtrl:setFont(GeneId_Label, Font),
  GeneIdTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(GeneIdTXT, Font),
  wxTextCtrl:setEditable(GeneIdTXT, false),

  Fit_Label = wxStaticText:new(MainPanel, ?wxID_ANY, "Best gen Score:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Fit_Label,100),
  wxTextCtrl:setFont(Fit_Label, Font),
  FitTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),
  wxTextCtrl:setFont(FitTXT, Font),
  wxTextCtrl:setEditable(FitTXT, false),


  Gen_Label = wxStaticText:new(MainPanel, ?wxID_ANY, "Genration:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Gen_Label,100),
  GenTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),

  wxTextCtrl:setFont(Gen_Label, Font),
  wxTextCtrl:setFont(GenTXT, Font),
  wxTextCtrl:setEditable(GenTXT, false),

  Died_label = wxStaticText:new(MainPanel, ?wxID_ANY, "Genes died:", [{style, ?wxALIGN_RIGHT}]),
  wxStaticText:wrap(Died_label,100),
  DiedTXT = wxTextCtrl:new(MainPanel, ?wxID_ANY, [{value, "***************"}, {style, ?wxTE_RIGHT}]),

  wxTextCtrl:setFont(Died_label, Font),
  wxTextCtrl:setFont(DiedTXT, Font),
  wxTextCtrl:setEditable(DiedTXT, false),

  %%% define the GUI containers
  %%%===================================================================

  StatsSizer = wxBoxSizer:new(?wxHORIZONTAL),
  NNSIzer = wxBoxSizer:new(?wxHORIZONTAL),


  wxSizer:add(StatsSizer, GeneId_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(StatsSizer, GeneIdTXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  wxSizer:add(StatsSizer, ActiveNodes_label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(StatsSizer, ActiveNodesTXT,    [{proportion,7},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  wxSizer:add(StatsSizer, Gen_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(StatsSizer, GenTXT,     [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(StatsSizer, Processes_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(StatsSizer, ProcessesTXT,     [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  wxSizer:add(NNSIzer, Fit_Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(NNSIzer, FitTXT, [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  wxSizer:add(NNSIzer, Work_label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(NNSIzer, WorkTXT, [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(NNSIzer, Neurons_label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(NNSIzer, NeuroTXT, [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(NNSIzer, Died_label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(NNSIzer, DiedTXT, [{proportion,1},{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

  wxSizer:add(MainSizer, StatsSizer, [{flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer, NNSIzer, [{flag, ?wxEXPAND}]),
  wxWindow:setSizer(MainFrame, MainSizer),
  wxWindow:setMinSize(MainFrame, wxWindow:getSize(MainFrame)),
  wxWindow:setMaxSize(MainFrame, wxWindow:getSize(MainFrame)),

  Vbox = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Vbox, MainPanel, [{flag, ?wxEXPAND}]),

  State = #state{frame = MainFrame, panel = MainPanel},

  %%% Callback function
  %%%===================================================================

  CallBackPaint =	fun(#wx{event = #wxPaint{}}, _wxObj)->
    case ets:lookup_element(stats, mode, 2)==startSimulation of
      true->
        case ets:lookup_element(stats, firstSimStarted, 2)==true of
          true->
            % destroy the stats UI if simulation started
            wxTextCtrl:destroy(ProcessesTXT),
            wxTextCtrl:destroy(NeuroTXT),
            wxTextCtrl:destroy(FitTXT),
            wxTextCtrl:destroy(GenTXT),
            wxTextCtrl:destroy(DiedTXT),
            wxTextCtrl:destroy(ActiveNodesTXT),
            wxTextCtrl:destroy(WorkTXT),
            wxTextCtrl:destroy(GeneIdTXT),

            wxStaticText:destroy(ActiveNodes_label),
            wxStaticText:destroy(Work_label),
            wxStaticText:destroy(GeneId_Label),
            wxStaticText:destroy(Neurons_label),
            wxStaticText:destroy(Processes_Label),
            wxStaticText:destroy(Fit_Label),
            wxStaticText:destroy(Gen_Label),
            wxStaticText:destroy(Died_label),
            ets:insert(stats,{firstSimStarted,false});
          false -> ok
        end,
        % show the Rabbit/hunter positions
        {Rabbit_pos,Rabbit_IMG} = ets:lookup_element(gui_db,rabbit,2),
        {Hunter_pos,Hunter_IMG} = ets:lookup_element(gui_db,hunter,2),
        Paint = wxBufferedPaintDC:new(MainPanel),
        SimBackground = wxBitmap:new("images/background.bmp"),
        wxDC:drawBitmap(Paint, SimBackground,{0,0}),
        wxBitmap:destroy(SimBackground),
        Rabbit_BMP = wxBitmap:new(Rabbit_IMG),
        wxDC:drawBitmap(Paint, Rabbit_BMP,Rabbit_pos),
        wxBitmap:destroy(Rabbit_BMP),
        Hunter_BMP = wxBitmap:new(Hunter_IMG),
        wxDC:drawBitmap(Paint, Hunter_BMP,Hunter_pos),
        wxBitmap:destroy(Hunter_BMP),
        wxBufferedPaintDC:destroy(Paint);
      false->
        % refresh statistics values
        wxTextCtrl:changeValue(DiedTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, gene_died	 , 2)]))),
        wxTextCtrl:changeValue(ActiveNodesTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, active	 , 2)]))),
        wxTextCtrl:changeValue(WorkTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, workload	 , 2)]))),
        wxTextCtrl:changeValue(ProcessesTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, process	 , 2)]))),
        wxTextCtrl:changeValue(FitTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, bestGeneScore , 2)]))),
        wxTextCtrl:changeValue(NeuroTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, neurons , 2)]))),
        wxTextCtrl:changeValue(GenTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, genration , 2)]))),
        wxTextCtrl:changeValue(GeneIdTXT, lists:flatten(io_lib:format("~p", [ets:lookup_element(stats, bestGeneID	 , 2)]))),
        wxPanel:refresh(DiedTXT),
        wxPanel:refresh(ActiveNodesTXT),
        wxPanel:refresh(WorkTXT),

        wxPanel:refresh(ProcessesTXT),
        wxPanel:refresh(FitTXT),
        wxPanel:refresh(NeuroTXT),
        wxPanel:refresh(GenTXT),
        wxPanel:refresh(GeneIdTXT),
        wxPanel:refresh(MainFrame), %refresh the panel

        Paint = wxBufferedPaintDC:new(MainPanel),
        LearningBackground = wxBitmap:new("images/learning.bmp"),
        wxDC:drawBitmap(Paint, LearningBackground,{0,0}),
        wxBitmap:destroy(LearningBackground),
        wxBufferedPaintDC:destroy(Paint)
    end end,


  wxFrame:connect(MainPanel, paint, [{callback, CallBackPaint}]),
  timer:send_interval(80, self(), graphicUpdate),

  wxFrame:show(MainFrame),
  {MainFrame, State}.

%%%=======================pick the right image for animation

handle_call({update_img, Rabbit_pos,Hunter_pos}, _From, State = #state{ panel = Panel}) ->
  ets:insert(stats,{mode,startSimulation}),
  Position = ets:lookup_element(stats, position, 2),
  case Position of
    1->ets:insert(gui_db,{rabbit,{Rabbit_pos,"images/dog1.bmp"}}),ets:insert(gui_db,{hunter,{Hunter_pos,"images/cat1.bmp"}}),
      ets:insert(stats,{position,Position+1});
    2->ets:insert(gui_db,{rabbit,{Rabbit_pos,"images/dog2.bmp"}}),ets:insert(gui_db,{hunter,{Hunter_pos,"images/cat2.bmp"}}),
      ets:insert(stats,{position,Position+1});
    3->ets:insert(gui_db,{rabbit,{Rabbit_pos,"images/dog3.bmp"}}),ets:insert(gui_db,{hunter,{Hunter_pos,"images/cat3.bmp"}}),
      ets:insert(stats,{position,Position+1});
    4->ets:insert(gui_db,{rabbit,{Rabbit_pos,"images/dog4.bmp"}}),ets:insert(gui_db,{hunter,{Hunter_pos,"images/cat4.bmp"}}),
      ets:insert(stats,{position,Position+1});
    5->ets:insert(gui_db,{rabbit,{Rabbit_pos,"images/dog5.bmp"}}),ets:insert(gui_db,{hunter,{Hunter_pos,"images/cat5.bmp"}}),
      ets:insert(stats,{position,Position+1});
    6->ets:insert(gui_db,{rabbit,{Rabbit_pos,"images/dog6.bmp"}}),ets:insert(gui_db,{hunter,{Hunter_pos,"images/cat6.bmp"}}),
      ets:insert(stats,{position,1})
  end,
  {reply, ok, State};



handle_call({statistics,Statistics}, _From, State = #state{}) ->
  ets:insert(stats,Statistics),
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  wxWindow:refresh(State#state.panel,[{eraseBackground,false}]),
  {noreply, State}.

handle_event(#wx{event = #wxClose{}},State = #state{frame = Frame}) -> % close window event
  io:format("Exiting\n"),
  wxWindow:destroy(Frame),
  wx:destroy(),
  {stop,normal,State};

handle_event(_Event,State) -> % when left click has been pressed, activate navigation function
  {noreply,State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.


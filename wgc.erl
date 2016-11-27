-module(wgc). %% wolf, goat and cabbage
-behaviour (gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([move/1, move/0, status/0, start_link/0]).

-record(state, {left=[], right=[], farmer_position}).
-define(SIDES, [left, right]).
-define(ELEMENTS, [wolf, goat, cabbage]).

%%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #state{left=?ELEMENTS, farmer_position=left}}.

%% move to other side without anything
move() ->
  FarmerPosition = gen_server:call(?MODULE, get_current_side),
  gen_server:call(?MODULE, {move, nil, other_side(FarmerPosition)}).
%% move to other side with something
move(What) ->
  FarmerPosition = gen_server:call(?MODULE, get_current_side),
  move(lists:member(What, ?ELEMENTS), What, other_side(FarmerPosition)).
move(false, What, _) ->
  io:format("Invalid element to move: ~p! Only ~p accepted!~n", [What, ?ELEMENTS]);
move(true, What, Where) ->
  gen_server:call(?MODULE, {move, What, Where}).

status() ->
  gen_server:call(?MODULE, status).

%% gen_server callbacks
handle_call({move, nil, _Where}, _From, S) ->
  process_move(S);
handle_call({move, What, Where}, _From, S=#state{farmer_position=FarmerPosition}) ->
  case validate_move(What, Where, S) of
    true ->
      {NewLeft, NewRight} = transfer(What, Where, S),
      process_move(#state{left=NewLeft, right=NewRight, farmer_position=FarmerPosition});
    false ->
      reply_cant_move(What, Where, S)
  end;
handle_call(status, _From, State) ->
  {reply, state_to_string(State), State};
handle_call(get_current_side, _From, S=#state{farmer_position=FarmerPosition}) ->
  {reply, FarmerPosition, S};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, State) ->
  io:format("~s~n", [state_to_string(State)]).

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

% private functions
process_move(S=#state{farmer_position=FarmerPosition}) ->
  NewState = S#state{farmer_position=other_side(FarmerPosition)},
  Reply = validate_state(NewState),
  reply_new_state(Reply, NewState).

other_side(left) -> right;
other_side(_)    -> left.

reply_new_state(failure, State) ->
  {stop, normal, "Game over: you lose!", State};
reply_new_state(success, State) ->
  {stop, normal, "Game over: you won!", State};
reply_new_state(ok, State) ->
  {reply, state_to_string(State), State}.

reply_cant_move(What, Where, S) ->
  {reply, lists:flatten(io_lib:format("Can't move to ~p~s! ~s", [Where, elements_to_string(What), state_to_string(S)])), S}.
elements_to_string(nil) ->
  [];
elements_to_string(What) ->
  lists:flatten(io_lib:format(" with ~p", [What])).

state_to_string(#state{left=Left, right=Right, farmer_position=FarmerPosition}) ->
  lists:flatten(io_lib:format("left: ~p, right: ~p, Farmer is at ~p side", [Left, Right, FarmerPosition])).

transfer(What, right, #state{left=Left, right=Right}) ->
  NewLeft = Left -- [What],
  NewRight = [What|Right],
  {NewLeft, NewRight};
transfer(What, left, #state{left=Left, right=Right}) ->
  NewLeft = [What|Left],
  NewRight = Right -- [What],
  {NewLeft, NewRight}.

%% move with something
validate_move(What, Where, #state{left=Left, right=Right, farmer_position=FarmerPosition}) ->
  validate_move(What, Where, Left, Right, Where =:= other_side(FarmerPosition)).
%% moving 'What' from Left to Right
validate_move(What, right, Left, _Right, true) ->
  lists:member(What, Left);
%% moving 'What' from Right to Left
validate_move(What, left, _Left, Right, true) ->
  lists:member(What, Right);
%% cannot move because Farmer is on the wrong side
validate_move(_,_,_,_,false) ->
  false.

select_side(Left, Right) ->
  {SideElementsCount, SideElements, SideName} = max({length(Left), Left, left}, {length(Right), Right, right}),
  {SideName, lists:sort(SideElements), SideElementsCount}.

%% everything was transfered to another side
validate_state(#state{left=[]}) ->
  success;
%% transfer in progress, check if player has failed
validate_state(#state{left=Left, right=Right, farmer_position=FarmerPosition}) ->
  %% only need to consider a side that have more elements
  {SideName, Elements, Count} = select_side(Left, Right),
  IsFarmerPresent = SideName =:= FarmerPosition,
  validate_state(Elements, Count, IsFarmerPresent).
validate_state(List, Count, IsFarmerPresent) when Count =:= 2 ->
  First  = lists:nth(1, List),
  Second = lists:nth(2, List),
  validate_side(First, Second, IsFarmerPresent);
validate_state(_, Count, false) when Count =:= 3 ->
  failure;
validate_state(_, Count, true) when Count =:= 3 ->
  ok.

validate_side(cabbage, goat, false) -> failure;
validate_side(goat, wolf, false)    -> failure;
validate_side(_, _, _)              -> ok.

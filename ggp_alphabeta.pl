update_state(State) :-
    retractall(true(_)),
    forall(member(X, State), assertz(true(X))).

findinits(Inits) :-
    setof(Init, init(Init), Inits).
    
findterminalp(State, Terminal) :-
    update_state(State),
    ((terminal, Terminal = true) ; 
     (\+terminal, Terminal = false)).
    
findreward(Role, State, Reward) :-
    update_state(State),
    goal(Role, Reward).

findlegals(Role, State, Legals) :-
    update_state(State),
    setof(does(Role, Legal), legal(Role, Legal), Legals).

findnext(State, Does, Next) :-
    update_state(State),
    retractall(does(_,_)),
    assertz(Does),
    setof(Proposition, next(Proposition), Next).

moves(State, Nexts) :-
    update_state(State),
    \+terminal,
    true(control(Role)),
    findlegals(Role, State, Legals),
    maplist(findnext(State), Legals, Nexts).
  
min_to_move(Pos) :-
  update_state(Pos),
  true(control(black)),!.

max_to_move(Pos) :-
  update_state(Pos),
  true(control(red)),!.

staticval(Pos, Val) :-
  update_state(Pos),
  terminal,
  goal(red, RedVal),
  goal(black, BlackVal),
  Val is RedVal - BlackVal.

heuristicval(Pos, Val) :-
  update_state(Pos),
  true(piece_count(red, RedVal)),
  true(piece_count(black, BlackVal)),
  Val is RedVal - BlackVal.

alphabeta(Pos, _, _, _, Val) :-
  get_time(Now),
  timeout(TimeOut),
  Now > TimeOut,
  heuristicval(Pos, Val), !.

alphabeta(Pos, Alpha, Beta, GoodPos, Val)  :-
  moves( Pos, PosList), !,
  boundedbest(PosList, Alpha, Beta, GoodPos, Val)
  ;
  staticval( Pos, Val).                              % Static value of Pos 

boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal)  :-
  alphabeta(Pos, Alpha, Beta, _, Val),
  goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

goodenough( [], _, _, Pos, Val, Pos, Val)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val)  :-
  min_to_move( Pos), Val > Beta, !                   % Maximizer attained upper bound
  ;
  max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal)  :-
  newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds  
  boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1),
  betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
  min_to_move( Pos), Val > Alpha, !.                 % Maximizer increased lower bound 

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
   max_to_move( Pos), Val < Beta, !.                 % Minimizer decreased upper bound 

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged 

betterof( Pos0, Val0, _Pos1, Val1, Pos0, Val0)  :-        % Pos0 better than Pos1
  min_to_move( Pos0),                                    % MIN to move in Pos0
  Val0 > Val1, !                                         % MAX prefers the greater value
  ;
  max_to_move( Pos0),                                    % MAX to move in Pos0
  Val0 < Val1, !.                                % MIN prefers the lesser value 

betterof( _Pos0, _Val0, Pos1, Val1, Pos1, Val1).           % Otherwise Pos1 better than Pos0

get_move(State, Next, Move) :-
  update_state(State),
  true(control(Role)),
  findlegals(Role, State, Legals),
  maplist(findnext(State), Legals, Nexts),
  nth0(Idx,Nexts,Next),
  nth0(Idx,Legals,Move).

bestmove(State, Bestmove) :-
  get_time(Now),
  TimeOut is Now + 5,
  retractall(timeout(_)),
  assertz(timeout(TimeOut)),
  alphabeta(State, -100, 100, Next, _Val),
  get_move(State, Next, Bestmove).



% Query rules as defined in notes formerly at 

update_state(State) :-
    retractall(true(_)),
    forall(member(X, State), assertz(true(X))).

findroles(Roles) :-
    findall(Role, role(Role), Roles).

findinits(Inits) :-
    setof(Init, init(Init), Inits).

findlegals(Role, State, Legals) :-
    update_state(State),
    setof(does(Role, Legal), legal(Role, Legal), Legals).
    
findlegals(Role, Legals) :-
    setof(does(Role, Legal), legal(Role, Legal), Legals).

findnext(Does, State, Next) :-
    update_state(State),
    retractall(does(_,_)),
    assertz(Does),
    setof(Proposition, next(Proposition), Next).

findnext(Does, Next) :-  % State already set
    retractall(does(_,_)),
    assertz(Does),
    setof(Proposition, next(Proposition), Next).

findterminalp(State, Terminal) :-
    update_state(State),
    ((terminal, Terminal = true) ; 
     (\+terminal, Terminal = false)).
    
findreward(Role, State, Reward) :-
    update_state(State),
    goal(Role, Reward).

/*
% create a random player to test UI

random_move(State, Move) :-
    update_state(State),
    true(control(Role)),
    setof(does(Role, Legal), legal(Role, Legal), Legals),
    random_member(Move, Legals).
*/

maxplayer(black).
timelimit(10).

triple(X, Y, Z, Result) :-
    Result = [X, Y, Z].
    
prod(X, Y, Result) :-
    Result is X * Y.
    
add(X, Y, Result) :-
    Result is X + Y.

eq([Maxval,_],[Maxval,_]).

utility(Utility) :-
    ((terminal,
      goal(black, Black),
      goal(red, Red),
      Utility is Black - Red)
     ;
     (\+terminal,
      true(piece_count(black, Black)),
      true(piece_count(red, Red)),
      Utility is Black - Red)).    

get_children(Children) :- % update_state(Node) has already been called
    true(control(Role)),
    findlegals(Role, Legals),
    maplist(findnext, Legals, Children).

timeouts(Moves, Timeouts) :-
    get_time(Now),
    length(Moves, Length),
    timelimit(Timelimit),
    Timeout is Timelimit / Length,
    findall(X, between(1, Length, X), Timeouts0),
    maplist(prod(Timeout), Timeouts0, Timeouts1),
    maplist(add(Now), Timeouts1, Timeouts).

minimax(Node, Timeout, BestValue) :-
    update_state(Node),
    get_time(Now),
    (((terminal ; Now >= Timeout),
      utility(BestValue))
     ;
     (\+terminal,
      Now < Timeout,
      get_children(Children),
      findall(BestValue1, 
            (member(Pos, Children),
             minimax(Pos, Timeout, BestValue1)), 
            BestValues),
      update_state(Node),
      maxplayer(Maxplayer),
      true(control(Role)),
      ((Maxplayer == Role,
        max_list(BestValues, BestValue))
       ;
       (Maxplayer \== Role,
        min_list(BestValues, BestValue)))
     )).

bestmove(State, Move) :-
    update_state(State),
    true(control(Role)),
    findlegals(Role, Legals),
    timeouts(Legals, Timeouts),
    maplist(findnext, Legals, Nexts),
    maplist(triple, Legals, Nexts, Timeouts, LegalNextList),
    findall([Value, Legal], (member([Legal, Next, Timeout], LegalNextList), minimax(Next, Timeout, Value)), ValueLegalList1),
    sort(1, @>=, ValueLegalList1, ValueLegalList2),
    ValueLegalList2 = [[Maxval,_]|_],
    include(eq([Maxval,_]), ValueLegalList2, ValueLegalList),
    random_member(ValueLegal, ValueLegalList),
    [_, Move] = ValueLegal.




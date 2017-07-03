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

findnext(Does, State, Next) :-
    update_state(State),
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

% create a random player to test UI

random_move(State, Move) :-
    update_state(State),
    true(control(Role)),
    setof(does(Role, Legal), legal(Role, Legal), Legals),
    random_member(Move, Legals).


% bestmove(State, Move)

aiplayer(black).

idf*([State|Open1], Depth1, Limit, Value) :-
    update_state(State),
    (\+terminal,
     Depth1 < Limit,
     true(control(Role)),
     findlegals(Role, State, Legals),
     findall(Next, (member(Legal, Legals), findnext(Legal, State, Next)), Nexts),
     append(Nexts, Open1, Open2),
     Depth2 is Depth1 + 1,
     idf*(Open2, Depth2, Limit, Value))
    ;
    (terminal,
     goal(red, Red),
     goal(black, Black),
     Value is Black - Red)
    ;
    (\+terminal,
     Depth1 == Limit,
     true(piece_count(red, Red)),
     true(piece_count(black, Black)),
     Value is Black - Red).

valuemove(State, Move, Value) :-
    % aiplayer(Role),
    update_state(State),
    findnext(Move, State, Next),
    idf*([Next], 0, 80, Value).

maxval(Val, Val/_).

bestmove(State, Action) :-
    update_state(State),
    true(control(Role)),
    findlegals(Role, State, Legals),
    setof(Value/Legal, (member(Legal, Legals), valuemove(State, Legal, Value)), Values1),
    reverse(Values1, Values2),
    [MaxVal/_|_] = Values2,
    sublist(maxval(MaxVal), Values2, Values3),
    random_member(_/Action, Values3).


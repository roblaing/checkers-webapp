% Query rules as defined in notes formerly at 

update_state(State) :-
    retractall(true(_)),
    forall(member(X, State), assertz(true(X))).

findroles(Roles) :-
    findall(Role, role(Role), Roles).

% State = [1].
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
    
findnexts(State, Nexts) :-
    update_state(State),
    \+terminal,
    role(Role),
    findlegals(Role, Legals),
    maplist(findnext(State), Legals, Nexts).

% create a random player to test UI

random_move(State, Move) :-
    update_state(State),
    true(control(Role)),
    findlegals(Role, State, Legals),
    random_member(Move, Legals).

% assertz(move(State, Next))   


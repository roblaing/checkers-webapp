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



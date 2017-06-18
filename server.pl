#!/usr/bin/env swipl
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).

:- initialization main.

:- http_handler('/', http_reply_file('index.html', []), []).
:- http_handler('/script.js', http_reply_file('script.js', []), []).
:- http_handler('/move', move_handler, []).

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

% http server stuff as explained at http://eu.swi-prolog.org/howto/http

move_handler(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [move=MoveString, state=StateString, aiplayer=AIplayerString|_], []),
    term_string(Move, MoveString),
    term_string(State, StateString),
    term_string(AIplayer, AIplayerString),
    findnext(Move, State, NextState),
    findterminalp(NextState, Terminal),
    ((\+Terminal,
    random_move(NextState, AIMove),
    findnext(AIMove, NextState, Next),
    findlegals(red, Next, Legals))
    ;
    (Terminal,
     Next = NextState,  
     Legals = noop)),
    findreward(red, NextState, Reward),
    format('Content-type: application/x-www-form-urlencoded~n~n', []),
    format('state=~w&legals=~w&aiplayer=~w&reward=~w&terminal=~w', [Next, Legals, AIplayer, Reward, Terminal]).

main :-
  consult('checkers.pl'),
  http_server(http_dispatch, [port(3000)]),
  thread_get_message(quit).


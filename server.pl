#!/usr/bin/env swipl
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).

:- initialization main.

:- http_handler('/', http_reply_file('index.html', []), []).
:- http_handler('/script.js', http_reply_file('script.js', []), []).
:- http_handler('/style.css', http_reply_file('style.css', []), []).
:- http_handler('/move', move_handler, []).

move_handler(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [move=MoveString, state=StateString, aiplayer=AIplayerString|_], []),
    term_string(Move, MoveString),
    term_string(State, StateString),
    term_string(AIplayer, AIplayerString),
    findnext(Move, State, NextState),
    findterminalp(NextState, Terminal),
    ((\+Terminal,
    % random_move(NextState, AIMove),
    bestmove(NextState, AIMove),
    findnext(AIMove, NextState, Next),
    findlegals(red, Next, Legals))
    ;
    (Terminal,
     Next = NextState,  
     Legals = noop)),
    findreward(red, NextState, Reward),
    format('Content-type: application/x-www-form-urlencoded~n~n', []),
    format('move=~w&state=~w&legals=~w&aiplayer=~w&reward=~w&terminal=~w', [AIMove, Next, Legals, AIplayer, Reward, Terminal]).

main :-
  consult('checkers.pl'),
  consult('ggp.pl'),
  consult('ggp_minimax.pl'),
  http_server(http_dispatch, [port(3000)]),
  thread_get_message(quit).


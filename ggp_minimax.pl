maxplayer(black).
timelimit(10).

utility(Node, Utility) :-
    update_state(Node),
    ((terminal,
      goal(black, Black),
      goal(red, Red),
      Utility is Black - Red)
     ;
     (\+terminal,
      true(piece_count(black, Black)),
      true(piece_count(red, Red)),
      Utility is Black - Red)).    

get_children(Node, Children) :- 
    update_state(Node),
    true(control(Role)),
    findlegals(Role, Node, Legals),
    maplist(findnext(Node), Legals, Children).
    
minimax(Depth1, Timeout, Node, Value) :-
    get_time(Now),
    update_state(Node),
    (Depth1 == 0; terminal ; Now >= Timeout),
    utility(Node, Value).

minimax(Depth1, Timeout, Node, Value) :-
    Depth1 >= 0,   % need separate rule for Depth1 == 0
    Depth2 is Depth1 - 1,
    update_state(Node),
    \+terminal,
    true(control(Player)),
    maxplayer(Maxplayer),
    get_children(Node, Children),
    maplist(minimax(Depth2, Timeout), Children, Values),
    ((Player == Maxplayer,
      max_list(Values, Value)) 
     ; 
     (Player \== Maxplayer,
      min_list(Values, Value))).

iterative_deepening(Depth1, Timeout, Node, Value) :-
   minimax(Depth1, Timeout, Node, Value),
   get_time(Now),
   ((Now >= Timeout ; Value == 100 ; Value == -100), !)
   ;
   Depth2 is Depth1 + 1,
   iterative_deepening(Depth2, Timeout, Node, Value).

prod(X, Y, Result) :-
    Result is X * Y.
    
add(X, Y, Result) :-
    Result is X + Y.
   
timeouts(Moves, Timeouts) :-
    get_time(Now),
    length(Moves, Length),
    timelimit(Timelimit),
    Timeout is Timelimit / Length,
    findall(X, between(1, Length, X), Timeouts0),
    maplist(prod(Timeout), Timeouts0, Timeouts1),
maplist(add(Now), Timeouts1, Timeouts).

tuple(A, B, [A, B]).
    
bestmove(State, Bestmove) :-
    update_state(State),
    true(control(Role)),
    findlegals(Role, State, Legals),
    maplist(findnext(State), Legals, Children),
    timeouts(Children, Timeouts),
    maplist(iterative_deepening(0), Timeouts, Children, Values),
    maplist(tuple, Values, Legals, Lst1),
    sort(1, @>=, Lst1, Lst2),
    [[Maxval, _]|_] = Lst2,
    include(=([Maxval,_]), Lst2, Lst3),
    random_member([_, Bestmove], Lst3).

/*   
main :-
    consult('checkers.pl'),
    consult('ggp.pl'),
    findinits(Start),
    update_state(Start),
    findnext(does(red,move(wp,h,3,g,4)), Next),
    bestmove(Next, Bestmove),
    writeln(Bestmove),
    update_state(Next),
    findlegals(black, Legals),
    writeln(Legals).
*/

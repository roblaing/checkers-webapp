% converted to prolog from http://games.ggp.org/base/games/checkers/checkers.kif
role(red).
role(black).

%*******************************************************************************
%* Initial state.                                                              *
%* Letters are columns: row 1 is RED side, row 8 is BLACK                      *
%* Numbers are rows:    column a is left, h is right (from red side)           *
%*******************************************************************************

init(cell(a, 1, b)).
init(cell(a, 3, b)).
init(cell(a, 4, b)).
init(cell(a, 5, b)).
init(cell(a, 7, b)).
init(cell(b, 2, b)).
init(cell(b, 4, b)).
init(cell(b, 5, b)).
init(cell(b, 6, b)).
init(cell(b, 8, b)).
init(cell(c, 1, b)).
init(cell(c, 3, b)).
init(cell(c, 4, b)).
init(cell(c, 5, b)).
init(cell(c, 7, b)).
init(cell(d, 2, b)).
init(cell(d, 4, b)).
init(cell(d, 5, b)).
init(cell(d, 6, b)).
init(cell(d, 8, b)).
init(cell(e, 1, b)).
init(cell(e, 3, b)).
init(cell(e, 4, b)).
init(cell(e, 5, b)).
init(cell(e, 7, b)).
init(cell(f, 2, b)).
init(cell(f, 4, b)).
init(cell(f, 5, b)).
init(cell(f, 6, b)).
init(cell(f, 8, b)).
init(cell(g, 1, b)).
init(cell(g, 3, b)).
init(cell(g, 4, b)).
init(cell(g, 5, b)).
init(cell(g, 7, b)).
init(cell(h, 2, b)).
init(cell(h, 4, b)).
init(cell(h, 5, b)).
init(cell(h, 6, b)).
init(cell(h, 8, b)).
init(cell(a, 2, wp)).
init(cell(b, 1, wp)).
init(cell(c, 2, wp)).
init(cell(d, 1, wp)).
init(cell(e, 2, wp)).
init(cell(f, 1, wp)).
init(cell(g, 2, wp)).
init(cell(h, 1, wp)).
init(cell(b, 3, wp)).
init(cell(d, 3, wp)).
init(cell(f, 3, wp)).
init(cell(h, 3, wp)).
init(cell(a, 8, bp)).
init(cell(c, 8, bp)).
init(cell(e, 8, bp)).
init(cell(g, 8, bp)).
init(cell(h, 7, bp)).
init(cell(f, 7, bp)).
init(cell(d, 7, bp)).
init(cell(b, 7, bp)).
init(cell(a, 6, bp)).
init(cell(c, 6, bp)).
init(cell(e, 6, bp)).
init(cell(g, 6, bp)).
init(control(red)).
init(step(1)).
init(piece_count(red, 12)).
init(piece_count(black, 12)).

%*******************************************************************************
%* NEXT STATE AXIOMS: REGULAR MOVES                                            *
%*******************************************************************************

% MOVE SOURCE
% Piece P moves out of U V leaving square blank

next(cell(U, V, b)) :- 
    does(_Player, move(_P, U, V, _X, _Y)).

next(cell(U, V, b)) :- 
    does(_Player, doublejump(_P, U, V, _X, _Y, _X3, _Y3)).

next(cell(U, V, b)) :- 
    does(_Player, triplejump(_P, U, V, _X, _Y, _X3, _Y3, _X4, _Y4)).

% MOVE DESTINATION: NON-KINGING MOVE
% Piece P moves to X Y
next(cell(X, Y, P)) :- 
    does(_Player, move(P, _U, _V, X, Y)), 
    (dif(P, wp) ; dif(Y, 8)), 
    (dif(P, bp) ; dif(Y, 1)).

next(cell(X, Y, P)) :- 
    does(_Player, doublejump(P, _U, _V, _X3, _Y3, X, Y)), 
    (dif(P, wp) ; dif(Y, 8)), 
    (dif(P, bp) ; dif(Y, 1)).

next(cell(X, Y, P)) :- 
    does(_Player, triplejump(P, _U, _V, _X3, _Y3, _X4, _Y4, X, Y)), 
    (dif(P, wp) ; dif(Y, 8)), 
    (dif(P, bp) ; dif(Y, 1)).
    
% UNDISTURBED CELL: NON-CAPTURE MOVE
% Piece (or blank) ?p at ?x ?y remains unchanged if:
% 1) This move is not a capture
% 2) ?x ?y is a different cell from the move source cell
% 3) ?x ?y is a different cell from the move destination cell
next(cell(X, Y, P)) :- 
    does(Player, move(_Piece, X1, Y1, X2, Y2)), 
    true(cell(X, Y, P)), 
    \+(single_jump_capture(Player, X1, Y1, X, Y, X2, Y2)), 
    different_cells(X, Y, X1, Y1), 
    different_cells(X, Y, X2, Y2).

next(cell(X, Y, P)) :- 
    does(Player, doublejump(_Piece, X1, Y1, X2, Y2, X3, Y3)), 
    true(cell(X, Y, P)), 
    \+(single_jump_capture(Player, X1, Y1, X, Y, X2, Y2)), 
    \+(single_jump_capture(Player, X2, Y2, X, Y, X3, Y3)), 
    different_cells(X, Y, X1, Y1), 
    different_cells(X, Y, X3, Y3).

next(cell(X, Y, P)) :- 
    does(Player, triplejump(_Piece, X1, Y1, X2, Y2, X3, Y3, X4, Y4)), 
    true(cell(X, Y, P)), 
    \+(single_jump_capture(Player, X1, Y1, X, Y, X2, Y2)), 
    \+(single_jump_capture(Player, X2, Y2, X, Y, X3, Y3)), 
    \+(single_jump_capture(Player, X3, Y3, X, Y, X4, Y4)), 
    different_cells(X, Y, X1, Y1), 
    different_cells(X, Y, X4, Y4).

% CAPTURED CELL
next(cell(X, Y, b)) :- 
    does(Player, move(_Piece, X1, Y1, X2, Y2)), 
    single_jump_capture(Player, X1, Y1, X, Y, X2, Y2).

next(cell(X, Y, b)) :- 
    does(Player, doublejump(_Piece, X1, Y1, X2, Y2, X3, Y3)), 
    (single_jump_capture(Player, X1, Y1, X, Y, X2, Y2) ; 
     single_jump_capture(Player, X2, Y2, X, Y, X3, Y3)).

next(cell(X, Y, b)) :- 
    does(Player, triplejump(_Piece, X1, Y1, X2, Y2, X3, Y3, X4, Y4)), 
    (single_jump_capture(Player, X1, Y1, X, Y, X2, Y2) ; 
     single_jump_capture(Player, X2, Y2, X, Y, X3, Y3) ; 
     single_jump_capture(Player, X3, Y3, X, Y, X4, Y4)).
     
% CONTROL TRANSFER
next(control(red)) :- true(control(black)).
next(control(black)) :- true(control(red)).

% MOVE COUNT
next(step(Y)) :- true(step(X)), succ(X, Y).

%*******************************************************************************
%* NEXT STATE AXIOMS: SPECIAL MOVES                                            *
%*******************************************************************************

% MOVE DESTINATION: KINGING MOVE
next(cell(X, 8, wk)) :- does(red, move(wp, _U, _V, X, 8)).
next(cell(X, 1, bk)) :- does(black, move(bp, _U, _V, X, 1)).
next(cell(X, 8, wk)) :- does(red, doublejump(wp, _U, _V, _X3, _Y3, X, 8)).
next(cell(X, 1, bk)) :- does(black, doublejump(bp, _U, _V, _X3, _Y3, X, 1)).
next(cell(X, 8, wk)) :- does(red, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, X, 8)).
next(cell(X, 1, bk)) :- does(black, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, X, 1)).

% NEXT for PIECE COUNTER
next(piece_count(Player, N)) :- 
    (does(Player, move(_P, _U, _V, _X, _Y)) ; 
     does(Player, doublejump(_P, _U, _V, _X3, _Y3, _X, _Y)) ; 
     does(Player, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, _X, _Y))), 
    true(piece_count(Player, N)).

next(piece_count(red, N)) :- 
    does(black, move(_P, X1, Y1, X2, Y2)), 
    kingmove(black, X1, Y1, X2, Y2), 
    true(piece_count(red, N)).

next(piece_count(red, Lower)) :- 
    does(black, move(_P, X1, Y1, X2, Y2)), 
    single_jump_capture(black, X1, Y1, _X, _Y, X2, Y2), 
    true(piece_count(red, Higher)), 
    minus1(Higher, Lower).

next(piece_count(red, Lower)) :- 
    does(black, doublejump(_P, _U, _V, _X3, _Y3, _X, _Y)), 
    true(piece_count(red, Higher)), 
    minus2(Higher, Lower).

next(piece_count(red, Lower)) :- 
    does(black, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, _X, _Y)), 
    true(piece_count(red, Higher)), 
    minus3(Higher, Lower).

next(piece_count(black, N)) :- 
    does(red, move(_P, X1, Y1, X2, Y2)), 
    kingmove(red, X1, Y1, X2, Y2), 
    true(piece_count(black, N)).

next(piece_count(black, Lower)) :- 
    does(red, move(_P, X1, Y1, X2, Y2)), 
    single_jump_capture(red, X1, Y1, _X, _Y, X2, Y2), 
    true(piece_count(black, Higher)), 
    minus1(Higher, Lower).

next(piece_count(black, Lower)) :- 
    does(red, doublejump(_P, _U, _V, _X3, _Y3, _X, _Y)), 
    true(piece_count(black, Higher)), 
    minus2(Higher, Lower).

next(piece_count(black, Lower)) :- 
    does(red, triplejump(_P, _U, _V, _X3, _Y3, _X4, _Y4, _X, _Y)), 
    true(piece_count(black, Higher)), 
    minus3(Higher, Lower).

%*******************************************************************************
%* LEGAL MOVES and their auxilliary axioms                                     *
%*******************************************************************************

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, pawn), 
    pawnmove(Player, U, V, X, Y), 
    true(cell(X, Y, b)).

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, king), 
    kingmove(Player, U, V, X, Y), 
    true(cell(X, Y, b)).

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, pawn), 
    pawnjump(Player, U, V, X, Y), 
    true(cell(X, Y, b)), 
    single_jump_capture(Player, U, V, _C, _D, X, Y).

legal(Player, move(Piece, U, V, X, Y)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, king), 
    kingjump(Player, U, V, X, Y), 
    true(cell(X, Y, b)), 
    single_jump_capture(Player, U, V, _C, _D, X, Y).

legal(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, pawn), 
    pawnjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2).
    
legal(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, king), 
    kingjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    kingjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2).

legal(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, pawn), 
    pawnjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    pawnjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    pawnjump(Player, X2, Y2, X3, Y3), 
    true(cell(X3, Y3, b)), 
    different_cells(X1, Y1, X3, Y3), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2), 
    single_jump_capture(Player, X2, Y2, _C2, _D2, X3, Y3).

legal(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    true(control(Player)), 
    true(cell(U, V, Piece)), 
    piece_owner_type(Piece, Player, king), 
    kingjump(Player, U, V, X1, Y1), 
    true(cell(X1, Y1, b)), 
    kingjump(Player, X1, Y1, X2, Y2), 
    true(cell(X2, Y2, b)), 
    different_cells(U, V, X2, Y2), 
    kingjump(Player, X2, Y2, X3, Y3), 
    true(cell(X3, Y3, b)), 
    different_cells(X1, Y1, X3, Y3), 
    single_jump_capture(Player, U, V, _C, _D, X1, Y1), 
    single_jump_capture(Player, X1, Y1, _C1, _D1, X2, Y2), 
    single_jump_capture(Player, X2, Y2, _C2, _D2, X3, Y3).

% NO-OPs for player not moving
legal(red, noop) :- true(control(black)).
legal(black, noop) :- true(control(red)).

% pawnmove
pawnmove(red, U, V, X, Y) :- 
    valid_cell(U, V), 
    next_rank(V, Y), 
    (next_file(U, X) ; next_file(X, U)).

pawnmove(black, U, V, X, Y) :- 
    valid_cell(U, V), 
    next_rank(Y, V), 
    (next_file(U, X) ; next_file(X, U)).

kingmove(Player, U, V, X, Y) :- 
    role(Player), 
    role(Player2), 
    pawnmove(Player2, U, V, X, Y).

pawnjump(red, U, V, X, Y) :- 
    valid_cell(U, V), 
    next_rank(V, V1), 
    next_rank(V1, Y), 
    next_file(U, X1), 
    next_file(X1, X).

pawnjump(red, U, V, X, Y) :- 
    valid_cell(U, V), 
    next_rank(V, V1), 
    next_rank(V1, Y), 
    next_file(X, X1), 
    next_file(X1, U).

pawnjump(black, U, V, X, Y) :- 
    valid_cell(U, V), 
    next_rank(Y, V1), 
    next_rank(V1, V), 
    next_file(U, X1), 
    next_file(X1, X).

pawnjump(black, U, V, X, Y) :- 
    valid_cell(U, V), 
    next_rank(Y, V1), 
    next_rank(V1, V), 
    next_file(X, X1), 
    next_file(X1, U).

kingjump(Player, U, V, X, Y) :- 
    role(Player), 
    role(Player2), 
    pawnjump(Player2, U, V, X, Y).

% single jump capture ?player means player is jumping from 
% u v to x y over c d, and an opponent's piece is at c d.

single_jump_capture(Player, U, V, C, D, X, Y) :- 
    kingjump(Player, U, V, X, Y), 
    kingmove(Player, U, V, C, D), 
    kingmove(Player, C, D, X, Y), 
    true(cell(C, D, Piece)), 
    opponent(Player, Opponent), 
    piece_owner_type(Piece, Opponent, _Type).

% Goals and Terminal

has_legal_move(Player) :- 
    piece_owner_type(Piece, Player, _Type), 
    (legal(Player, move(Piece, _U, _V, _X, _Y)) ; 
     legal(Player, doublejump(Piece, _U, _V, _X1, _Y1, _X, _Y)) ; 
     legal(Player, triplejump(Piece, _U, _V, _X1, _Y1, _X2, _Y2, _X, _Y))).

stuck(Player) :- 
    role(Player), 
    \+(has_legal_move(Player)).

terminal :- 
    true(control(Player)), 
    stuck(Player).
    
terminal :- true(piece_count(_Player, 0)).

terminal :- true(step(102)).

goal(red, 100) :- 
    true(piece_count(red, Rc)), 
    true(piece_count(black, Bc)), 
    greater(Rc, Bc).

goal(red, 50) :- 
    true(piece_count(red, X)), 
    true(piece_count(black, X)).

goal(red, 0) :- 
    true(piece_count(red, Rc)), 
    true(piece_count(black, Bc)), 
    greater(Bc, Rc).

goal(black, 100) :- 
    true(piece_count(red, Rc)), 
    true(piece_count(black, Bc)), 
    greater(Bc, Rc).

goal(black, 50) :- 
    true(piece_count(red, X)), 
    true(piece_count(black, X)).

goal(black, 0) :- 
    true(piece_count(red, Rc)), 
    true(piece_count(black, Bc)), 
    greater(Rc, Bc).

%*******************************************************************************
% AUXILIARY PREDICATES                                                         *
%*******************************************************************************

%;;  DIFFERENT CELLS
%;;  True iff ?x1 ?y1 is a different cell from ?x2 ?y2

adjacent(X1, X2) :- next_file(X1, X2).
adjacent(X1, X2) :- next_file(X2, X1).
adjacent(Y1, Y2) :- next_rank(Y1, Y2).
adjacent(Y1, Y2) :- next_rank(Y2, Y1).

different_cells(X1, Y1, X2, Y2) :- dif(X1, X2), valid_cell(X1, Y1), valid_cell(X2, Y2).
different_cells(X1, Y1, X2, Y2) :- dif(Y1, Y2), valid_cell(X1, Y1), valid_cell(X2, Y2).

% PLAYER OPPONENTS
opponent(red, black).
opponent(black, red).

% PIECE OWNERSHIP AND TYPE 
piece_owner_type(wk, red, king).
piece_owner_type(wp, red, pawn).
piece_owner_type(bk, black, king).
piece_owner_type(bp, black, pawn).

% BOARD TOPOLOGY
next_file(a, b).
next_file(b, c).
next_file(c, d).
next_file(d, e).
next_file(e, f).
next_file(f, g).
next_file(g, h).
next_rank(1, 2).
next_rank(2, 3).
next_rank(3, 4).
next_rank(4, 5).
next_rank(5, 6).
next_rank(6, 7).
next_rank(7, 8).
file1(a).
file1(c).
file1(e).
file1(g).
file2(b).
file2(d).
file2(f).
file2(h).
file(X) :- file1(X).
file(X) :- file2(X).
rank1(1).
rank1(3).
rank1(5).
rank1(7).
rank2(2).
rank2(4).
rank2(6).
rank2(8).
rank(Y) :- rank1(Y).
rank(Y) :- rank2(Y).
valid_cell(X, Y) :- file1(X), rank2(Y).
valid_cell(X, Y) :- file2(X), rank1(Y).
greater(A, B) :- minus1(A, B).
greater(A, B) :- dif(A, B), succ(C, A), greater(C, B).
minus3(12, 9).
minus3(11, 8).
minus3(10, 7).
minus3(9, 6).
minus3(8, 5).
minus3(7, 4).
minus3(6, 3).
minus3(5, 2).
minus3(4, 1).
minus3(3, 0).
minus2(12, 10).
minus2(11, 9).
minus2(10, 8).
minus2(9, 7).
minus2(8, 6).
minus2(7, 5).
minus2(6, 4).
minus2(5, 3).
minus2(4, 2).
minus2(3, 1).
minus2(2, 0).
minus1(12, 11).
minus1(11, 10).
minus1(10, 9).
minus1(9, 8).
minus1(8, 7).
minus1(7, 6).
minus1(6, 5).
minus1(5, 4).
minus1(4, 3).
minus1(3, 2).
minus1(2, 1).
minus1(1, 0).
count(N) :- minus1(N, _Nm1).
count(0).

% MOVE COUNT SUCCESSOR
succ(0, 1).
succ(1, 2).
succ(2, 3).
succ(3, 4).
succ(4, 5).
succ(5, 6).
succ(6, 7).
succ(7, 8).
succ(8, 9).
succ(9, 10).
succ(10, 11).
succ(11, 12).
succ(12, 13).
succ(13, 14).
succ(14, 15).
succ(15, 16).
succ(16, 17).
succ(17, 18).
succ(18, 19).
succ(19, 20).
succ(20, 21).
succ(21, 22).
succ(22, 23).
succ(23, 24).
succ(24, 25).
succ(25, 26).
succ(26, 27).
succ(27, 28).
succ(28, 29).
succ(29, 30).
succ(30, 31).
succ(31, 32).
succ(32, 33).
succ(33, 34).
succ(34, 35).
succ(35, 36).
succ(36, 37).
succ(37, 38).
succ(38, 39).
succ(39, 40).
succ(40, 41).
succ(41, 42).
succ(42, 43).
succ(43, 44).
succ(44, 45).
succ(45, 46).
succ(46, 47).
succ(47, 48).
succ(48, 49).
succ(49, 50).
succ(50, 51).
succ(51, 52).
succ(52, 53).
succ(53, 54).
succ(54, 55).
succ(55, 56).
succ(56, 57).
succ(57, 58).
succ(58, 59).
succ(59, 60).
succ(60, 61).
succ(61, 62).
succ(62, 63).
succ(63, 64).
succ(64, 65).
succ(65, 66).
succ(66, 67).
succ(67, 68).
succ(68, 69).
succ(69, 70).
succ(70, 71).
succ(71, 72).
succ(72, 73).
succ(73, 74).
succ(74, 75).
succ(75, 76).
succ(76, 77).
succ(77, 78).
succ(78, 79).
succ(79, 80).
succ(80, 81).
succ(81, 82).
succ(82, 83).
succ(83, 84).
succ(84, 85).
succ(85, 86).
succ(86, 87).
succ(87, 88).
succ(88, 89).
succ(89, 90).
succ(90, 91).
succ(91, 92).
succ(92, 93).
succ(93, 94).
succ(94, 95).
succ(95, 96).
succ(96, 97).
succ(97, 98).
succ(98, 99).
succ(99, 100).
succ(100, 101).
succ(101, 102).
succ(102, 103).
succ(103, 104).
succ(104, 105).
succ(105, 106).
succ(106, 107).
succ(107, 108).
succ(108, 109).
succ(109, 110).
succ(110, 111).
succ(111, 112).
succ(112, 113).
succ(113, 114).
succ(114, 115).
succ(115, 116).
succ(116, 117).
succ(117, 118).
succ(118, 119).
succ(119, 120).
succ(120, 121).
succ(121, 122).
succ(122, 123).
succ(123, 124).
succ(124, 125).
succ(125, 126).
succ(126, 127).
succ(127, 128).
succ(128, 129).
succ(129, 130).
succ(130, 131).
succ(131, 132).
succ(132, 133).
succ(133, 134).
succ(134, 135).
succ(135, 136).
succ(136, 137).
succ(137, 138).
succ(138, 139).
succ(139, 140).
succ(140, 141).
succ(141, 142).
succ(142, 143).
succ(143, 144).
succ(144, 145).
succ(145, 146).
succ(146, 147).
succ(147, 148).
succ(148, 149).
succ(149, 150).
succ(150, 151).
succ(151, 152).
succ(152, 153).
succ(153, 154).
succ(154, 155).
succ(155, 156).
succ(156, 157).
succ(157, 158).
succ(158, 159).
succ(159, 160).
succ(160, 161).
succ(161, 162).
succ(162, 163).
succ(163, 164).
succ(164, 165).
succ(165, 166).
succ(166, 167).
succ(167, 168).
succ(168, 169).
succ(169, 170).
succ(170, 171).
succ(171, 172).
succ(172, 173).
succ(173, 174).
succ(174, 175).
succ(175, 176).
succ(176, 177).
succ(177, 178).
succ(178, 179).
succ(179, 180).
succ(180, 181).
succ(181, 182).
succ(182, 183).
succ(183, 184).
succ(184, 185).
succ(185, 186).
succ(186, 187).
succ(187, 188).
succ(188, 189).
succ(189, 190).
succ(190, 191).
succ(191, 192).
succ(192, 193).
succ(193, 194).
succ(194, 195).
succ(195, 196).
succ(196, 197).
succ(197, 198).
succ(198, 199).
succ(199, 200).
succ(200, 201).

%;; Base and input relations
base(cell(X, Y, b)) :- file(X), rank(Y).
base(cell(X, Y, Piece)) :- valid_cell(X, Y), piece_owner_type(Piece, _Owner, _Type).
base(control(Player)) :- role(Player).
base(step(N)) :- succ(_Nm1, N).
base(piece_count(Player, N)) :- role(Player), count(N).
input(Player, noop) :- role(Player).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, pawn), pawnmove(Player, U, V, X, Y).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, king), kingmove(Player, U, V, X, Y).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, pawn), pawnjump(Player, U, V, X, Y).
input(Player, move(Piece, U, V, X, Y)) :- piece_owner_type(Piece, Player, king), kingjump(Player, U, V, X, Y).
input(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    piece_owner_type(Piece, Player, pawn), pawnjump(Player, U, V, X1, Y1), pawnjump(Player, X1, Y1, X2, Y2).
input(Player, doublejump(Piece, U, V, X1, Y1, X2, Y2)) :- 
    piece_owner_type(Piece, Player, king), 
    kingjump(Player, U, V, X1, Y1), 
    kingjump(Player, X1, Y1, X2, Y2), 
    different_cells(U, V, X2, Y2).
input(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    piece_owner_type(Piece, Player, pawn), 
    pawnjump(Player, U, V, X1, Y1), 
    pawnjump(Player, X1, Y1, X2, Y2), different_cells(U, V, X2, Y2), pawnjump(Player, X2, Y2, X3, Y3), different_cells(X1, Y1, X3, Y3).
input(Player, triplejump(Piece, U, V, X1, Y1, X2, Y2, X3, Y3)) :- 
    piece_owner_type(Piece, Player, king), 
    kingjump(Player, U, V, X1, Y1), 
    kingjump(Player, X1, Y1, X2, Y2), 
    different_cells(U, V, X2, Y2), 
    kingjump(Player, X2, Y2, X3, Y3), 
    different_cells(X1, Y1, X3, Y3).


:- module grid.

% this is about letter grids and placing words

:- interface.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.

:- import_module util.

:- type size ---> size(width :: int, height :: int).
:- type point == {int, int}.
:- type grid ---> grid(size :: size, map :: map(point, char)).

:- pred init(size::in, grid::out) is det.
:- pred in_bounds(grid::in, point::in) is semidet.
:- func points(grid) = set(point) is det.
:- pred char_at(grid::in, point::in, char::out) is semidet.

% set character at point to the given value, if there's nothing there
% yet or the same character is already there
:- pred place_char(point::in, char::in, grid::in, grid::out) is semidet.

:- type dir == {int, int}.
:- func dirs = list(dir).

% place the given word in the given direction, if no conflict;
% sets output point to end of word
:- pred place_word(point::in, dir::in, word::in,
                   grid::in, grid::out, point::out) is semidet.

% place the given word in the given direction, placing the given
% character (at any place in the word) at the given point;
% return start and end point
:- pred place_word_char(point::in, dir::in, word::in, char::in,
                        grid::in, grid::out, point::out, point::out) is nondet.

% variants that try any direction
:- pred place_word_any(point::in, word::in,
                       grid::in, grid::out, point::out) is nondet.
:- pred place_word_char_any(point::in, word::in, char::in,
                            grid::in, grid::out, point::out, point::out) is nondet.

:- implementation.

:- import_module maybe.
:- import_module solutions.

init(Size, Grid) :- map.init(M),
                    Grid = grid(Size, M).

in_bounds(Grid, {X, Y}) :- X >= 0,
                           Y >= 0,
                           X < Grid^size^width,
                           Y < Grid^size^height.

:- pred nondet_in_bounds(grid::in, point::out) is nondet.
nondet_in_bounds(Grid, P) :- int.nondet_int_in_range(0, Grid^size^width - 1, X),
                             int.nondet_int_in_range(0, Grid^size^height - 1, Y),
                             P = {X, Y}.

points(Grid) = solutions_set(pred(P::out) is nondet :- nondet_in_bounds(Grid, P)).

char_at(G, P, C) :- in_bounds(G, P),
                    C = map.search(G^map, P).

place_char(P, C, Gin, Gout) :- in_bounds(Gin, P),
                               grid(S, M) = Gin,
                               map.search_insert(P, C, OldC, M, M1),
                               (OldC = no; OldC = yes(C)),
                               Gout = grid(S, M1).

dirs = solutions((pred(D::out) is nondet :- int.nondet_int_in_range(-1, 1, DX),
                                            int.nondet_int_in_range(-1, 1, DY),
                                            (DX \= 0; DY \= 0),
                                            D = {DX, DY}
                 )).

:- func move(dir, point) = point.
move({DX, DY}, {PX, PY}) = {PX + DX, PY + DY}.

:- func rev(dir) = dir.
rev({DX, DY}) = {-DX, -DY}.

place_word(P, _, [C], Gin, Gout, Pend) :- place_char(P, C, Gin, Gout),
                                          Pend = P.
place_word(P, D, [C|[C1|Cs]], Gin, Gout, Pend) :- place_char(P, C, Gin, G),
                                                  P1 = move(D, P),
                                                  place_word(P1, D, [C1|Cs], G, Gout, Pend).

place_word_char(P, D, W, C, Gin, Gout, Pstart, Pend) :-
    util.split_word(W, C, W0, W1),
    place_word(P, rev(D), W0, Gin, G, Pstart),
    place_word(P, D, W1, G, Gout, Pend).

place_word_any(P, W, Gin, Gout, Pend) :-
    list.member(D, dirs),
    place_word(P, D, W, Gin, Gout, Pend).

place_word_char_any(P, W, C, Gin, Gout, Pstart, Pend) :-
    list.member(D, dirs),
    place_word_char(P, D, W, C, Gin, Gout, Pstart, Pend).

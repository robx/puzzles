:- module grid.

% this is about letter grids and placing words

:- interface.

:- import_module char.
:- import_module int.
:- import_module map.
:- import_module set.

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

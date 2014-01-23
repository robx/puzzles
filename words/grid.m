:- module grid.

% this is about letter grids and placing words

:- interface.

:- import_module char.
:- import_module int.
:- import_module map.

:- type size ---> size(height :: int, width :: int).
:- type point == {int, int}.
:- type grid ---> grid(size :: size, map :: map(point, char)).

:- pred init(size::in, grid::out) is det.
:- pred in_bounds(grid::in, point::in) is semidet.

:- implementation.

init(Size, Grid) :- map.init(M),
                    Grid = grid(Size, M).

in_bounds(Grid, {X, Y}) :- X >= 0,
                           Y >= 0,
                           X < Grid^size^width,
                           Y < Grid^size^height.

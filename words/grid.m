:- module grid.

% this is about letter mgrids and placing words

:- interface.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.

:- import_module util.

:- type size ---> size(width :: int, height :: int).
:- type point == {int, int}.
:- type dir == {int, int}.
:- func dirs = list(dir).

:- typeclass grid(T) where [
    func size(T) = size,
    pred char_at(T::in, point::in, char::out) is semidet,

    % set character at point to the given value, if there's nothing there
    % yet or the same character is already there
    pred place_char(point::in, char::in, T::in, T::out) is semidet
].

:- pred in_bounds(T::in, point::in) is semidet <= (grid(T)).
:- func points(T) = set(point) is det <= (grid(T)).
:- pred place_chars(list({point, char}):: in, T::in, T::out) is semidet <= (grid(T)).

% place the given word in the given direction, if no conflict;
% sets output point to end of word
:- pred place_word(point::in, dir::in, word::in,
                   T::in, T::out, point::out) is semidet <= (grid(T)).

% place the given word in the given direction, placing the given
% character (at any place in the word) at the given point;
% return start and end point
:- pred place_word_char(point::in, dir::in, word::in, char::in,
                        T::in, T::out, point::out, point::out) is nondet <= (grid(T)).

% variants that try any direction
:- pred place_word_any(point::in, word::in,
                       T::in, T::out, point::out) is nondet <= (grid(T)).
:- pred place_word_char_any(point::in, word::in, char::in,
                            T::in, T::out, point::out, point::out) is nondet <= (grid(T)).

:- func show(T) = list(string) <= (grid(T)).

:- type mgrid ---> mgrid(msize :: size, mmap :: map(point, char)).

:- instance grid(mgrid).

:- pred m_init(size::in, mgrid::out) is det.

:- implementation.

:- import_module maybe.
:- import_module solutions.
:- import_module string.

:- instance grid(mgrid) where [
    size(G) = G^msize,
    (char_at(G, P, C) :- in_bounds(G, P),
                         C = map.search(G^mmap, P)),
    (place_char(P, C, Gin, Gout) :- in_bounds(Gin, P),
                                    mgrid(S, M) = Gin,
                                    map.search_insert(P, C, OldC, M, M1),
                                    (OldC = no; OldC = yes(C)),
                                    Gout = mgrid(S, M1))
].

m_init(Size, Grid) :- map.init(M),
                      Grid = mgrid(Size, M).

in_bounds(Grid, {X, Y}) :- X >= 0,
                           Y >= 0,
                           size(W, H) = size(Grid),
                           X < W,
                           Y < H.

:- pred nondet_in_bounds(T::in, point::out) is nondet <= (grid(T)).
nondet_in_bounds(Grid, P) :- size(W, H) = size(Grid),
                             int.nondet_int_in_range(0, W - 1, X),
                             int.nondet_int_in_range(0, H - 1, Y),
                             P = {X, Y}.

points(Grid) = solutions_set(pred(P::out) is nondet :- nondet_in_bounds(Grid, P)).

place_chars([], Gin, Gout) :- Gout = Gin.
place_chars([{P, C}|Xs], Gin, Gout) :- place_char(P, C, Gin, G1),
                                       place_chars(Xs, G1, Gout).

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

:- func int_range(int) = list(int).
int_range(N) = (if N =< 0 then [] else [N - 1|int_range(N - 1)]).

:- func show_char(T, point) = char <= (grid(T)).
show_char(G, P) = C :-
    solutions(pred(X::out) is nondet :- char_at(G, P, X),
              Cs),
    (
        Cs = [C | _]
    ;
        Cs = [],
        C = ('.')
    ).

:- func show_line(T, int) = string <= (grid(T)).
show_line(G, Y) = string.from_char_list(map(func(X) = show_char(G, {X, Y}),
                                            list.reverse(int_range(G^size^width)))).

show(G) = map(func(Y) = show_line(G, Y),
              int_range(G^size^height)).

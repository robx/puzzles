:- module solve.

:- interface.

:- import_module char.
:- import_module list.

:- import_module grid.
:- import_module util.

:- type hint == {point, char}.

:- pred solve(list(word)::in, list(hint)::in, size::in, hgrid::out) is nondet.

:- implementation.

solve(Ws, [H | Hs], S, Gout) :-
    {P0, C} = H,
    h_init(S, Hs, G),
    pick_cont(Ws, C, W0, Ws1),
    place_word_char_any(P0, W0, C, G, G1, Ps, Pe),
    solve1(Ws1, Ps, Pe, G1, Gout),
    h_no_hints(Gout).
    % TODO: check that no more than two words connect at any point

:- pred solve1(list(word)::in, point::in, point::in, T::in, T::out)
                                         is nondet <= (grid(T)).
solve1([], Ps, Pe, G, Gout) :- Pe = Ps,
                               Gout = G.
solve1(Ws, Ps, Pe, G, Gout) :-
    char_at(G, Ps, C),
    pick_start(Ws, C, W, Ws1),
    place_word_any(Ps, W, G, G1, Pn),
    solve1(Ws1, Pn, Pe, G1, Gout).


:- module unittests.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module solutions.

:- import_module util.
:- import_module examples.
:- import_module grid.
:- import_module solve.

main(!IO) :-
      Tests = ["test_pick_1_t" - test_pick_1_t,
               "test_pick_1_x" - test_pick_1_x,
               "test_pick_cont_1_n" - bool.pred_to_bool(test_pick_cont_1_n),
               "test_grid_points" - bool.pred_to_bool(test_grid_points),
               "test_grid_bounds" - bool.pred_to_bool(test_grid_bounds),
               "test_place_char" - bool.pred_to_bool(test_place_char),
               "test_char_at" - bool.pred_to_bool(test_char_at),
               "test_dirs" - bool.pred_to_bool(test_dirs),
               "test_place_word" - bool.pred_to_bool(test_place_word),
               "test_split_word" - bool.pred_to_bool(test_split_word),
               "test_place_word_char" - bool.pred_to_bool(test_place_word_char),
               "test_place_word_any" - bool.pred_to_bool(test_place_word_any),
               "test_place_word_char_any" - bool.pred_to_bool(test_place_word_char_any),
               "test_solve_1" - bool.pred_to_bool(test_solve_1)],
      io.write(Tests, !IO),
      io.write_string("\n", !IO),
      Status = bool_to_int(bool.not(and_snd(Tests))),
      io.set_exit_status(Status, !IO).

:- func and_snd(list(pair(string, bool))) = bool.
and_snd(Ts) = bool.and_list(list.map(pair.snd, Ts)).

:- func bool_to_int(bool) = int is det.
bool_to_int(yes) = 1.
bool_to_int(no) = 0.

% helper function to test 'pick_start'
:- pred pick_1_t(string::out) is nondet.
pick_1_t(S) :- examples.words_1(Xs),
               util.pick_start(Xs, 't', R, _),
               util.to_string(R, S).

:- func test_pick_1_t = bool.
test_pick_1_t = X :- if solutions_set(pick_1_t, S),
                        S = set.set(["tew", "tint", "tnit", "tnua"])
                     then X = yes
                     else X = no.

:- pred pick_1_x(string::out) is nondet.
pick_1_x(S) :- examples.words_1(Xs),
               util.pick_start(Xs, 'x', R, _),
               util.to_string(R, S).

:- func test_pick_1_x = bool.
test_pick_1_x = X :- if solutions_set(pick_1_x, S),
                        S = set.set([])
                     then X = yes
                     else X = no.

:- pred test_pick_cont_1_n is semidet.
test_pick_cont_1_n :- solutions_set(
                          pred(S::out) is nondet :- (
                              examples.words_1(Xs),
                              util.pick_cont(Xs, 'n', R, _),
                              util.to_string(R, S)
                          ),
                          Sols
                      ),
                      Sols = set.set(["an", "aunt", "tint", "win"]).

:- pred test_grid_points is semidet.
test_grid_points :- init(size(0, 0), G),
                    points(G) = set.set([]),
                    init(size(-2, 5), G1),
                    points(G1) = set.set([]),
                    init(size(1, 1), G2),
                    points(G2) = set.set([{0, 0}]),
                    init(size(2, 2), G3),
                    points(G3) = set.set([{0, 0}, {0, 1}, {1, 0}, {1, 1}]),
                    init(size(1, 2), G4),
                    points(G4) = set.set([{0, 0}, {0, 1}]).

:- pred test_grid_bounds is semidet.
test_grid_bounds :- init(size(3, 3), G),
                    solutions_set(
                        pred(P::out) is nondet :- (
                            int.nondet_int_in_range(-2, 5, X),
                            int.nondet_int_in_range(-2, 5, Y),
                            P = {X, Y},
                            in_bounds(G, P)
                        ),
                        Pin
                    ),
                    Pin = points(G).

:- pred test_place_char is semidet.
test_place_char :- init(size(2, 2), G),
                   place_char({0, 0}, 'A', G, G1),
                   place_char({1, 0}, 'B', G1, G2),
                   not place_char({1, 0}, 'C', G2, _),
                   place_char({0, 0}, 'A', G2, G3),
                   not place_char({2, 0}, 'D', G3, _).

:- pred test_char_at is semidet.
test_char_at :- init(size(2, 1), G),
                not char_at(G, {0, 0}, _),
                not char_at(G, {1, 1}, _),
                place_char({0, 0}, 'A', G, G1),
                char_at(G1, {0, 0}, 'A').

:- pred test_dirs is semidet.
test_dirs :- list.length(dirs) = 8,
             list.member({1, 0}, dirs),
             list.member({-1, -1}, dirs),
             not list.member({0, 0}, dirs),
             not list.member({2, 0}, dirs).

:- pred test_place_word is semidet.
test_place_word :- init(size(3, 2), G),
                   W1 = from_string("a"),
                   W2 = from_string("ba"),
                   W3 = from_string("abc"),
                   not place_word({2, 0}, {1, 0}, W3, G, _, _),
                   not place_word({3, 0}, {1, 0}, W1, G, _, _),
                   not place_word({0, 0}, {1, -1}, W2, G, _, _),
                   place_word({0, 0}, {1, -1}, W1, G, G1, {0, 0}),
                   char_at(G1, {0, 0}, 'a'),
                   not place_word({0, 0}, {1, 1}, W2, G1, _, _),
                   place_word({1, 0}, {-1, 1}, W2, G1, G2, {0, 1}),
                   char_at(G2, {0, 1}, 'a'),
                   place_word({0, 0}, {1, 0}, W3, G2, G3, {2, 0}),
                   char_at(G3, {2, 0}, 'c').

:- pred test_split_word is semidet.
test_split_word :- W = from_string("abdefg"),
                   split_word(W, 'b', from_string("ba"), from_string("bdefg")),
                   not split_word(W, 'h', _, _),
                   W1 = from_string("abac"),
                   solutions_set(pred(R::out) is nondet :- (
                                     split_word(W1, 'a', Ws, We),
                                     R = {Ws, We}
                                 ),
                                 Sols),
                   Sols = set.set([{from_string("a"), from_string("abac")},
                                   {from_string("aba"), from_string("ac")}]).

:- pred test_place_word_char is semidet.
test_place_word_char :-
    init(size(1, 5), G),
    W = from_string("abac"),
    place_char({0, 0}, 'c', G, G1),
    place_word_char({0, 0}, {0, -1}, W, 'c', G1, _, {0, 3}, {0, 0}),
    place_word_char({0, 1}, {0, -1}, W, 'a', G1, _, {0, 3}, {0, 0}),
    place_word_char({0, 1}, {0, 1}, W, 'a', G1, _, {0, 1}, {0, 4}),
    not place_word_char({0, 4}, {0, -1}, W, 'b', G1, _, _, _),
    place_word_char({0, 4}, {0, 1}, W, 'c', G1, _, {0, 1}, {0, 4}),
    not place_word_char({0, 3}, {0, 1}, W, 'c', G1, _, _, _).

:- pred test_place_word_any is semidet.
test_place_word_any :-
    init(size(3, 3), G),
    W = from_string("abc"),
    place_char({2, 0}, 'd', G, G1),
    solutions_set(pred(Pend::out) is nondet :- place_word_any({0, 0}, W, G1, _, Pend),
                  Ps),
    Ps = set.set([{2, 2}, {0, 2}]).

:- pred test_place_word_char_any is semidet.
test_place_word_char_any :-
    init(size(3, 3), G),
    W = from_string("abc"),
    place_char({2, 0}, 'd', G, G1),
    place_char({1, 2}, 'a', G1, G2),
    solutions_set(pred(Pstart::out) is nondet :-
                      place_word_char_any({1, 1}, W, 'b', G2, _, Pstart, _),
                  Ps),
    Ps = set.set([{0, 0}, {2, 1}, {2, 2}, {1, 2}, {0, 1}]).

:- pred test_solve_1 is semidet.
test_solve_1 :-
    S = size(3, 3),
    Ws = [from_string("abc")],
    Hs = [{{0, 0}, 'a'}],
    not solve(Ws, Hs, S, _),
    Ws1 = map(from_string, ["abc", "ccd", "dda"]),
    solve(Ws1, Hs, S, _).

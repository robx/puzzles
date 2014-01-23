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

main(!IO) :-
      Tests = ["test_pick_1_t" - test_pick_1_t,
               "test_pick_1_x" - test_pick_1_x,
               "test_pick_cont_1_n" - bool.pred_to_bool(test_pick_cont_1_n),
               "test_grid_points" - bool.pred_to_bool(test_grid_points),
               "test_grid_bounds" - bool.pred_to_bool(test_grid_bounds),
               "test_place_char" - bool.pred_to_bool(test_place_char),
               "test_char_at" - bool.pred_to_bool(test_char_at)],
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

:- module unittests.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module solutions.
:- import_module util.
:- import_module examples.

main(!IO) :-
      Tests = ["test_pick_1_t" - test_pick_1_t,
               "test_pick_1_x" - test_pick_1_x,
               "test_pick_cont_1_n" - bool.pred_to_bool(test_pick_cont_1_n)],
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

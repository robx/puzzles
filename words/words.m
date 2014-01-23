:- module words.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.
:- import_module solutions.
:- import_module util.
:- import_module examples.

main(!IO) :-
      solutions(test, R),
      io.write(R, !IO),
      io.write_string("\n", !IO).

% helper function to test 'pick'
:- pred test(string::out) is nondet.
test(S) :- examples.words_1(Xs),
           util.pick(Xs, 't', R, _),
           util.to_string(R, S).

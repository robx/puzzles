:- module words.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.
:- import_module solutions.

main(!IO) :-
      solutions(test, R),
      io.write(R, !IO),
      io.write_string("\n", !IO).

% helper function to test 'pick'
:- pred test(list(char)::out) is nondet.
test(R) :- words(Xs),
           pick(Xs, 't', R, _).

% mapping string.to_char_list directly yields compiler
% errors that probably need some kind of annotation to fix
% properly; this works, too
:- pred to_list(string::in, list(char)::out) is det.
to_list(A, B) :- string.to_char_list(A, B).

% sample puzzle
:- pred words(list(list(char))::out) is det.
words(Xs) :- Ws = ["an", "aunt", "tint", "wet", "win"],
             map(to_list, Ws, Xs).

% pick one from a list of words, optionally reversed,
% that starts with the given letter
:- pred pick(list(list(char))::in, char::in, 
             list(char)::out, list(list(char))::out) is nondet.
pick([X | Xs], C, X, Xs)       :- X = [C | _].
pick([X | Xs], C, Xrev, Xs)    :- Xrev = list.reverse(X),
                                  Xrev = [C | _].
pick([X | Xs], C, Y, [X | Ys]) :- pick(Xs, C, Y, Ys).

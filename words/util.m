:- module util.
:- interface.

:- import_module char.
:- import_module list.

% pick one from a list of words, optionally reversed,
% that starts with the given letter
:- pred pick(list(list(char))::in, char::in, 
             list(char)::out, list(list(char))::out) is nondet.

:- implementation.

pick([X | Xs], C, X, Xs)       :- X = [C | _].
pick([X | Xs], C, Xrev, Xs)    :- Xrev = list.reverse(X),
                                  Xrev = [C | _].
pick([X | Xs], C, Y, [X | Ys]) :- pick(Xs, C, Y, Ys).

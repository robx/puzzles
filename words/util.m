:- module util.
:- interface.

:- import_module char.
:- import_module list.
:- import_module string.

% pick one from a list of words, optionally reversed,
% that starts with the given letter
:- pred pick(list(list(char))::in, char::in, 
             list(char)::out, list(list(char))::out) is nondet.

% convert between lists of strings and lists of lists of chars.
:- pred to_string(list(char)::in, string::out) is det.
:- pred from_string(string::in, list(char)::out) is det.
:- pred to_strings(list(list(char))::in, list(string)::out) is det.
:- pred from_strings(list(string)::in, list(list(char))::out) is det.

:- implementation.

pick([X | Xs], C, X, Xs)       :- X = [C | _].
pick([X | Xs], C, Xrev, Xs)    :- Xrev = list.reverse(X),
                                  Xrev = [C | _].
pick([X | Xs], C, Y, [X | Ys]) :- pick(Xs, C, Y, Ys).


to_string(A, B) :- string.to_char_list(B, A).
from_string(A, B) :- string.to_char_list(A, B).
to_strings(Xs, Ys) :- list.map(to_string, Xs, Ys).
from_strings(Xs, Ys) :- list.map(from_string, Xs, Ys).

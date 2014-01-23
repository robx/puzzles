:- module examples.
:- interface.

:- import_module char.
:- import_module list.

:- pred words_1(list(list(char))::out) is det.

:- implementation.

:- import_module string.

% mapping string.to_char_list directly yields compiler
% errors that probably need some kind of annotation to fix
% properly; this works, too
:- pred to_list(string::in, list(char)::out) is det.
to_list(A, B) :- string.to_char_list(A, B).

% sample puzzle
words_1(Xs) :- Ws = ["an", "aunt", "tint", "wet", "win"],
               map(to_list, Ws, Xs).

:- module examples.
:- interface.

:- import_module char.
:- import_module list.

:- pred words_1(list(list(char))::out) is det.

:- implementation.

:- import_module string.
:- import_module util.

% sample puzzle
words_1(Xs) :- Ws = ["an", "aunt", "tint", "wet", "win"],
               util.from_strings(Ws, Xs).

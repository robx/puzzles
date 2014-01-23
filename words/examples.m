:- module examples.
:- interface.

:- import_module list.
:- import_module util.

:- pred words_1(list(word)::out) is det.

:- implementation.

:- import_module string.

% sample puzzle
words_1(Xs) :- Ws = ["an", "aunt", "tint", "wet", "win"],
               util.from_strings(Ws, Xs).

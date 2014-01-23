:- module examples.
:- interface.

:- import_module list.

:- import_module grid.
:- import_module util.
:- import_module solve.

:- func size_1 = size.
:- func words_1 = list(word).
:- func hints_1 = list(hint).

:- implementation.

:- import_module string.

% sample puzzle
size_1 = size(4, 4).
words_1 = Xs :- Ws = ["an", "aunt", "tint", "wet", "win"],
                util.from_strings(Ws, Xs).
hints_1 = [{{0, 1}, 'n'}, {{3, 2}, 'e'}].

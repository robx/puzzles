:- module examples.
:- interface.

:- import_module list.

:- import_module grid.
:- import_module util.
:- import_module solve.

:- func size_1 = size.
:- func words_1 = list(word).
:- func hints_1 = list(hint).

:- func size_2 = size.
:- func words_2 = list(word).
:- func hints_2 = list(hint).

:- implementation.

:- import_module string.

% sample puzzles

size_1 = size(4, 4).
words_1 = Xs :- Ws = ["an", "aunt", "tint", "wet", "win"],
                util.from_strings(Ws, Xs).
hints_1 = [{{3, 2}, 'e'}, {{0, 1}, 'n'}].

size_2 = size(11, 11).
words_2 = util.from_strings(["berlin", "bremen", "iksan",
                             "london", "lyon", "nantes",
                             "napoli", "nimes"]).
%hints_2 = [{{1, 2}, 'r'}, {{6, 2}, 'a'}, {{5, 9}, 'a'}, {{9, 2}, 'o'}].
hints_2 = [{{1, 1}, 'e'}, {{1, 2}, 'r'},
           {{9, 1}, 'y'}, {{9, 2}, 'o'},
           {{4, 4}, 'o'}, {{4, 5}, 'e'}].

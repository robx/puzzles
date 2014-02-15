:- module util.
:- interface.

:- import_module char.
:- import_module list.
:- import_module string.

:- type word == list(char).

% pick one from a list of words, optionally reversed,
% that starts with the given letter
:- pred pick_start(list(word)::in, char::in,
                   word::out, list(word)::out) is nondet.

% pick one from a list of words that contains the given letter
:- pred pick_cont(list(word)::in, char::in,
                  word::out, list(word)::out) is nondet.

% split a word at a given character, returning the reverse
% start and the end (both including that character).
:- pred split_word(word::in, char::in,
                   word::out, word::out) is nondet.

% convert between lists of strings and lists of lists of chars.
:- pred to_string(word::in, string::out) is det.
:- pred from_string(string::in, word::out) is det.
:- func from_string(string) = word.
:- pred to_strings(list(word)::in, list(string)::out) is det.
:- func from_strings(list(string)) = list(word).
:- pred from_strings(list(string)::in, list(word)::out) is det.

:- implementation.

pick_start([X | Xs], C, X, Xs)       :- X = [C | _].
pick_start([X | Xs], C, Xrev, Xs)    :- Xrev = list.reverse(X),
                                        Xrev = [C | _].
pick_start([X | Xs], C, Y, [X | Ys]) :- pick_start(Xs, C, Y, Ys).

pick_cont([X | Xs], C, X, Xs)       :- list.member(C, X).
pick_cont([X | Xs], C, Y, [X | Ys]) :- pick_cont(Xs, C, Y, Ys).

:- pred split_word1(word::in, word::in, char::in, word::out, word::out).
split_word1(_, [], _, _, _) :- false.
split_word1(W0, [C | W1], C, [C | W0], [C | W1]).
split_word1(W0, [C1 | W1], C, Wstart, Wend) :-
    split_word1([C1 | W0], W1, C, Wstart, Wend).

split_word(W, C, Wstart, Wend) :- split_word1([], W, C, Wstart, Wend).

to_string(A, B) :- string.to_char_list(B, A).
from_string(A, B) :- string.to_char_list(A, B).
from_string(A) = B :- from_string(A, B).
to_strings(Xs, Ys) :- list.map(to_string, Xs, Ys).
from_strings(Xs, Ys) :- list.map(from_string, Xs, Ys).
from_strings(Xs) = Ys :- from_strings(Xs, Ys).

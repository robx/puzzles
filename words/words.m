:- module words.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module string.
:- import_module solutions.
:- import_module util.
:- import_module examples.
:- import_module solve.
:- import_module grid.

main(!IO) :-
      S = examples.size_2,
      Ws = examples.words_2,
      Hs = examples.hints_2,
      do_while(pred(G::out) is nondet :- solve(Ws, Hs, S, G),
               get_next,
               !IO).

:- pred get_next(hgrid::in, bool::out, io::di, io::uo) is det.
get_next(G, More, !IO) :-
    foldl(write_line, show(G), !IO),
    io.write_string("\n", !IO),
    More = yes.

:- pred write_line(string::in, io::di, io::uo) is det.
write_line(L, !IO) :- io.format("%s\n", [s(L)], !IO).

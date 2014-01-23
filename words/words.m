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
      S = examples.size_1,
      Ws = examples.words_1,
      Hs = examples.hints_1,
      do_while(pred(G::out) is nondet :- solve(Ws, Hs, S, G),
               get_next,
               !IO).

:- pred get_next(mgrid::in, bool::out, io::di, io::uo) is det.
get_next(G, More, !IO) :-
    write_lines(show(G), !IO),
    io.write_string("\n", !IO),
    More = yes.

:- pred write_lines(list(string)::in, io::di, io::uo) is det.
write_lines([], !IO).
write_lines([L|Ls], !IO) :- io.write_string(L, !IO),
                            io.write_string("\n", !IO),
                            write_lines(Ls, !IO).

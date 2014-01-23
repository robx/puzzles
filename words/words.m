:- module words.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

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
      solutions(pred(G::out) is nondet :- solve(Ws, Hs, S, G),
                Gs),
      io.write_string("found ", !IO),
      io.write(list.length(Gs) : int, !IO),
      io.write_string(" solutions\n", !IO),
      show_sols(Gs, !IO).

:- pred show_sols(list(grid)::in, io::di, io::uo) is det.
show_sols([], !IO).
show_sols([G|Gs], !IO) :- write_lines(show(G), !IO),
                          io.write_string("\n", !IO),
                          show_sols(Gs, !IO).

:- pred write_lines(list(string)::in, io::di, io::uo) is det.
write_lines([], !IO).
write_lines([L|Ls], !IO) :- io.write_string(L, !IO),
                            io.write_string("\n", !IO),
                            write_lines(Ls, !IO).

:- module lint.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module package, analysis.
:- import_module hash_table, list, string, pair.

:- type command
    --->    duplicates
    ;       tag_counts.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [Filename, CmdStr],
        command(CmdStr, Command)
    then
        load_packages(Res, Filename, !IO),
        (
            Res = ok(Ps),
            run(Command, Ps, !IO)
        ;
            Res = error(E),
            io.format(io.stderr_stream, "failed to read packages from %s: %s\n",
                [s(Filename), s(error_message(E))], !IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.progname_base("lint", Progname, !IO),
        io.format(io.stderr_stream, "usage: %s <package filename> [dups|tag-counts]\n",
            [s(Progname)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred command(string, command).
:- mode command(in, out) is semidet.
command("dups", duplicates).
command("tag-counts", tag_counts).

:- pred run(command::in, list(package)::in, io::di, io::uo) is det.
run(duplicates, Ps, !IO) :-
    duplicate_packages(Ps, Dups),
    ( if not Dups = [] then
        io.write_string("Duplicate packages:\n", !IO),
        foldl((pred(Name::in, !.IO::di, !:IO::uo) is det :-
            io.format("\t%s\n", [s(Name)], !IO)), Dups, !IO)
    else
        io.write_string("No duplicates found.\n", !IO)
    ).
run(tag_counts, Ps, !IO) :-
    count_tags(Ps, TagTable),
    Tags = sort((func(_ - A, _ - B) = R :- compare(R, A, B)), to_assoc_list(TagTable)),
    foldl((pred((Tag - Count)::in, !.IO::di, !:IO::uo) is det :-
        io.format("%4d %s\n", [i(Count), s(Tag)], !IO)), Tags, !IO).

:- module postpackage.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module package, string, list, char, int, maybe.

:- func index_file = string.
index_file = "unreviewed.list".

main(!IO) :-
    io.get_environment_var("REQUEST_METHOD", Env, !IO),
    ( if Env = yes("POST") then
        % XXX: vulnerable to excessively long lines
        io.read_line_as_string(Res, !IO),
        ( if
            Res = ok(Line),
            no_php(Line),
            Term0 = chomp(Line),
            io.read_from_string("postbody", Term0, length(Term0),
                ok(Package), io.posn(0, 0, 0), _)
        then
            validate(tidy(Package), Valid),
            Term = string(tidy(Package)),
            (
                Valid = ok,
                io.open_append(index_file, Res2, !IO),
                ( if Res2 = ok(Stream) then
                    io.format(Stream, "%s.\n", [s(Term)], !IO),
                    close_output(Stream, !IO),
                    io.format("Content-type: text\n\npackage added: %s.\n",
                        [s(Term)], !IO)
                else
                    die(!IO)
                )
            ;
                Valid = error(Reason),
                badrequest(Reason, !IO)
            )
        else
            badrequest("couldn't parse package term", !IO)
        )
    else
        die(!IO)
    ).

    % no_php(String)
    %
    % Don't rely on well-configured servers that won't try to execute inline
    % PHP code in unreviewed.list
:- pred no_php(string::in) is semidet.
no_php(Line) :-
    not string.sub_string_search(Line, "<?", _).

:- pred badrequest(string::in, io::di, io::uo) is det.
badrequest(Reason, !IO) :-
    io.format("Status: 400 %s\n\n%s", [s(Reason), s(Reason)], !IO).

:- pred die(io::di, io::uo) is erroneous.
:- pragma foreign_proc("C",
    die(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    exit(1);
").

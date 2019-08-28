:- module mmcget.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module package, manager, solutions.
:- import_module list, string, exception, bool, int.
:- use_module ioextra, dir.

:- type reviewed ---> reviewed(package) ; unreviewed(package).
:- inst unreviewed ---> unreviewed(ground).

:- func version = string.
version = "v0.1.0".

:- pred usage(io::di, io::uo) is det.
usage(!IO) :-
    io.progname_base("mmc-get", Program, !IO),
    io.format(io.stderr_stream,
        "usage: %s update              # update mmc-get package lists\n" ++
        "usage: %s version\n" ++
        "usage: %s list [<filter>]     # list packages, with optional filter\n" ++
        "usage: %s listall [<filter>]  # ... including unreviewed packages\n" ++
        "usage: %s brief [<filter>]    # ... with less information\n" ++
        "usage: %s briefall [<filter>]\n" ++
        "usage: %s get <name>          # clone package into the working directory\n",
        [
            s(Program), s(Program), s(Program), s(Program),
            s(Program), s(Program), s(Program)
        ], !IO),
    io.set_exit_status(1, !IO).

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = ["update"] then
        update_packages(!IO),
        io.call_system("curl https://mercury-in.space/packages/news", _, !IO)
    else if Args = ["version"] then
        io.format("mmc-get version %s\n", [s(version)], !IO)
    else if Args = ["list"] then
        solutions(grep_reviewed(""), L),
        foldl(write_package, L, !IO)
    else if Args = ["list", Filter] then
        solutions(grep_reviewed(Filter), L),
        foldl(write_package, L, !IO)
    else if Args = ["brief"] then
        solutions(grep_reviewed(""), L),
        foldl(write_brief_package, L, !IO)
    else if Args = ["brief", Filter] then
        solutions(grep_reviewed(Filter), L),
        foldl(write_brief_package, L, !IO)
    else if Args = ["listall"] then
        solutions(grep(""), L),
        foldl(write_package, L, !IO)
    else if Args = ["listall", Filter] then
        solutions(grep(Filter), L),
        foldl(write_package, L, !IO)
    else if Args = ["briefall"] then
        solutions(grep(""), L),
        foldl(write_brief_package, L, !IO)
    else if Args = ["briefall", Filter] then
        solutions(grep(Filter), L),
        foldl(write_brief_package, L, !IO)
    else if Args = ["get", Want] then
        ( if solutions(exact(to_lower(Want)), [reviewed(Package)]) then
            clone(Package^vcs, Package, !IO)
        else
            solutions(want(Want), L),
            ( if L = [reviewed(Package)] then
                clone(Package^vcs, Package, !IO)
            else if L = [unreviewed(Package)] then
                write_package(unreviewed(Package), !IO),
                ask_to_clone(unreviewed(Package), !IO)
            else if L = [] then
                io.format(io.stderr_stream, "No package matched ``%s''\n",
                    [s(Want)], !IO),
                io.set_exit_status(1, !IO)
            else
                show_choices(dump_package, 1, L, !IO),
                choose(Res, L, !IO),
                (
                    Res = ok(reviewed(Package)),
                    clone(Package^vcs, Package, !IO)
                ;
                    Res = ok(unreviewed(Package)),
                    write_package(unreviewed(Package), !IO),
                    ask_to_clone(unreviewed(Package), !IO)
                ;
                    ( Res = eof ; Res = error(_) ),
                    io.set_exit_status(1, !IO)
                )
            )
        )
    else if Args = ["help"] then
        usage(!IO)
    else
        io.format(io.stderr_stream, "didn't understand these args: %s\n",
            [s(string(Args))], !IO),
        usage(!IO)
    ).

:- pred ask_to_clone(reviewed::in(unreviewed), io::di, io::uo) is det.
ask_to_clone(unreviewed(Package), !IO) :-
    yn_prompt("Really get unreviewed package?", B, !IO),
    (
        B = yes,
        clone(Package^vcs, Package, !IO)
    ;
        B = no,
        io.set_exit_status(1, !IO)
    ).

:- pred write_package(reviewed::in, io::di, io::uo) is det.
write_package(P, !IO) :-
    io.write_string(dump_package(P), !IO).

:- pred write_brief_package(reviewed::in, io::di, io::uo) is det.
write_brief_package(P, !IO) :-
    io.write_string(dump_brief_package(P), !IO).

:- func dump_brief_package(reviewed) = string.
dump_brief_package(reviewed(P)) =
    string.format("%-30s    %s\n", [s(P^name), s(P^description)]).
dump_brief_package(unreviewed(P)) =
    string.format("%-30s !! %s\n", [s(P^name), s(P^description)]).

:- func dump_package(reviewed) = string.
dump_package(reviewed(P)) = string.format("
Name             : %s
URL              : %s
Tags             : %s
License          : %s
Builds           : %s
Dependencies     : %s
Version Control  : %s (releases: %s)
FFI              : %s
Description      : %s
", [
    s(P^name), s(P^url), s(join_list(" ", P^tags)), s(string(P^license)),
    s(join_list(", ", P^apps ++ P^libs)), s(string(P^deps)), s(string(P^vcs)),
    s(string(P^release)), s(string(P^foreign)), s(P^description)
]).
dump_package(unreviewed(P)) = string.format("
Name (UNREVIEWED)? %s
URL              ? %s
Tags             ? %s
License          ? %s
Builds           ? %s
Dependencies     ? %s
Version Control  : %s (releases: %s)
FFI              ? %s
Description      ? %s
", [
    s(P^name), s(P^url), s(join_list(" ", P^tags)), s(string(P^license)),
    s(join_list(", ", P^apps ++ P^libs)), s(string(P^deps)), s(string(P^vcs)),
    s(string(P^release)), s(string(P^foreign)), s(P^description)
]).

:- pred grep_reviewed(string::in, reviewed::out) is nondet.
grep_reviewed(Substr, R @ reviewed(P)) :-
    packages(P),
    sub_string_search(to_lower(dump_package(R)), to_lower(Substr), _).

:- pred grep(string::in, reviewed::out) is nondet.
grep(Substr, R) :-
    ( packages(P), R = reviewed(P) ; unreviewed(P), R = unreviewed(P) ),
    sub_string_search(to_lower(dump_package(R)), to_lower(Substr), _).

:- pred want(string::in, reviewed::out) is nondet.
want(Substr, R) :-
    ( packages(P), R = reviewed(P) ; unreviewed(P), R = unreviewed(P) ),
    sub_string_search(to_lower(P^name), to_lower(Substr), _).

:- pred exact(string::in, reviewed::out) is nondet.
exact(Name, R) :-
    ( packages(P), R = reviewed(P) ; unreviewed(P), R = unreviewed(P) ),
    to_lower(P^name, Name).

:- pred clone(vcs::in, package::in, io::di, io::uo) is det.
clone(git, Package, !IO) :-
    ioextra.spawn("git", ["clone", Package^url, Package^name], Res, !IO),
    (
        Res = ok(Pid),
        ioextra.waitpid(Pid, Res2, !IO),
        (
            Res2 = ok,
            ioextra.chdir(Package^name, _, !IO),
            release(git, Package^release, Package, !IO)
        ;
            Res2 = error(_),
            io.write_string(io.stderr_stream, "`git clone' failed; is git in PATH?\n", !IO),
            throw(Res2)
        )
    ;
        Res = error(_),
        throw(Res)
    ).
clone(fossil, Package, !IO) :-
    Target = dir.(Package^name / ".fossil"),
    ioextra.exists(Target, Exists, !IO),
    (
        Exists = no,
        dir.make_directory(Package^name, Res1, !IO),
        (
            Res1 = ok,
            ioextra.spawn("fossil", ["clone", Package^url, Target], Res2, !IO),
            (
                Res2 = ok(Pid),
                ioextra.waitpid(Pid, Res, !IO),
                (
                    Res = ok,
                    ioextra.chdir(Package^name, _, !IO),
                    ioextra.spawn("fossil", ["open", ".fossil"], Res3, !IO),
                    (
                        Res3 = ok(Pid2),
                        ioextra.waitpid(Pid2, _, !IO),
                        release(fossil, Package^release, Package, !IO)
                    ;
                        Res3 = error(_),
                        throw(Res3)
                    )
                ;
                    Res = error(_),
                    io.write_string(io.stderr_stream, "`fossil clone' failed; is fossil in PATH?\n", !IO),
                    throw(Res)
                )
            ;
                Res2 = error(_),
                throw(Res2)
            )
        ;
            Res1 = error(_),
            throw(Res1)
        )
    ;
        Exists = yes,
        throw(Target ++ " already exists")
    ).

:- pred release(vcs::in, releases::in, package::in, io::di, io::uo) is det.
release(_, head, _, !IO).
release(_VCS, tag, _Package, !IO).
    %tags.tags(Tags, VCS, Package, !IO),
    %tags.checkout(tag.latest(Tags), VCS, Package, !IO).

:- pred yn_prompt(string::in, bool::out, io::di, io::uo) is det.
yn_prompt(S, B, !IO) :-
    io.write_string(S ++ " [y/n] ", !IO),
    io.read_line_as_string(Res, !IO),
    ( if
        Res = ok(Line),
        to_lower(chomp(Line)) = Ans
    then
        ( if Ans = "yes" ; Ans = "y" then
            B = yes
        else if Ans = "no" ; Ans = "n" then
            B = no
        else
            io.write_string("Sorry, I didn't understand that.\n\n", !IO),
            yn_prompt(S, B, !IO)
        )
    else
        throw(Res)
    ).

:- pred choose(io.result(reviewed), list(reviewed), io, io).
:- mode choose(out, in, di, uo) is det.
choose(R, Ps, !IO) :-
    io.format("Ambiguous result. Your choice? [1..%d, show, brief, q] ",
        [i(length(Ps))], !IO),
    io.read_line_as_string(Res, !IO),
    (
        Res = ok(Line),
        to_lower(chomp(Line)) = Choice,
        ( if
            to_int(Choice, N),
            index1(Ps, N, R0)
        then
            R = ok(R0)
        else if Choice = "show" then
            show_choices(dump_package, 1, Ps, !IO),
            choose(R, Ps, !IO)
        else if Choice = "brief" then
            show_choices(dump_brief_package, 1, Ps, !IO),
            choose(R, Ps, !IO)
        else if Choice = "q" then
            R = eof
        else
            io.write_string("Sorry, I didn't understand that.\n", !IO),
            choose(R, Ps, !IO)
        )
    ;
        Res = eof, R = eof
    ;
        Res = error(E), R = error(E)
    ).

:- pred show_choices(func(reviewed) = string, int, list(reviewed), io, io).
:- mode show_choices(in, in, in, di, uo) is det.
show_choices(_, _, [], !IO) :- io.nl(!IO).
show_choices(Dump, N, [P | Ps], !IO) :-
    io.format("[%d] %s", [i(N), s(Dump(P))], !IO),
    show_choices(Dump, N + 1, Ps, !IO).

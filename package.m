:- module package.
:- interface.
:- import_module list, io.

:- type licenses
    --->    gpl
    ;       bsd
    ;       mit
    ;       other.
:- type vcs
    --->    git
    ;       fossil.
:- type releases
    --->    head
    ;       tag.
:- type foreign
    --->    c
    ;       erlang
    ;       java
    ;       csharp.
:- type package
    --->    package(
                name :: string,
                license :: licenses,
                vcs :: vcs,
                release :: releases,
                url :: string,
                tags :: list(string),
                libs :: list(string),
                apps :: list(string),
                deps :: list(string),
                foreign :: list(foreign),
                description :: string,
                comment :: string
            ).
:- type valid_result
    --->    ok
    ;       error(string).

    % validate(Package, Result)
    %
    % presently this just rejects some special characters
    %
:- pred validate(package::in, valid_result::out) is det.

    % tidy(Package0) = Package
    %
    % trims strings and removes zero-length strings from lists.
    % TODO: normalize UTF8
:- func tidy(package) = package.

    % load_packages(PackagesResult, Filename, !IO)
    %
    % Produce a list of packages from a file containing one package term on
    % each line.
    %
:- pred load_packages(io.res(list(package))::out, string::in,
    io::di, io::uo) is det.

    % load_packages_from_stream(PackagesResult, Stream, !IO)
:- pred load_packages_from_stream(io.res(list(package))::out,
    io.text_input_stream::in,
    io::di, io::uo) is det.

%----------------------------------------------------------------------%
:- implementation.
:- import_module string, char, int.

load_packages(Res, Filename, !IO) :-
    io.open_input(Filename, Res0, !IO),
    (
        Res0 = ok(Stream),
        load_packages_from_stream(Res, Stream, !IO),
        io.close_input(Stream, !IO)
    ;
        Res0 = error(E),
        Res = error(E)
    ).

load_packages_from_stream(Res, Stream, !IO) :-
    load_packages_from_stream([], 1, Res, Stream, !IO).

:- pred load_packages_from_stream(
    list(package)::in,
    int::in,
    io.res(list(package))::out,
    io.text_input_stream::in,
    io::di, io::uo).
load_packages_from_stream(Acc, LineNo, Res, Stream, !IO) :-
    io.read_line_as_string(Stream, Res0, !IO),
    (
        Res0 = ok(Line),
        Term = chomp(Line),
        io.read_from_string("packages file", Term, length(Term),
            Res1, io.posn(LineNo, 0, 0), _),
        (
            Res1 = ok(Package),
            load_packages_from_stream([Package | Acc], LineNo + 1, Res, Stream, !IO)
        ;
            Res1 = eof,
            Res = ok(Acc)
        ;
            Res1 = error(Reason, N),
            Error = string.format("%s on line %d",
                [s(Reason), i(N)]),
            Res = error(make_io_error(Error))
        )
    ;
        Res0 = eof,
        Res = ok(Acc)
    ;
        Res0 = error(E),
        Res = error(E)
    ).

validate(P, R) :-
    ( if not all_match(valid_char, string(P)) then
        R = error("package term contains control chars (newline, ESC, NUL, etc.)")
    else
        R = ok
    ).

:- pred valid_char(char::in) is semidet.
valid_char(C) :-
    not (
        to_int(C) < to_int(' ')  % control chars, \n, etc.
    ).

tidy(package(Name, L, V, R, Url, Tags, Libs, Apps, Deps, F, Desc, Comment)) =
    package(strip(Name), L, V, R, strip(Url),
        nz(Tags), nz(Libs), nz(Apps), nz(Deps),
        F, strip(Desc), strip(Comment)).

    % remove zero-length strings from lists of strings
:- func nz(list(string)) = list(string).
nz(L) = negated_filter(unify(""), L).

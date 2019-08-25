:- module ioextra.
:- interface.
:- import_module io, list, bool.

    % spawn(Cmd, Args, PidResult, !IO)
    %
    % Spawn a subprocess that runs independently from the calling process.
    % PATH is used to resolve Cmd to an executable, if required.
:- pred spawn(string::in, list(string)::in, io.res(int)::out, io::di, io::uo) is det.

    % waitpid(Pid, Res, !IO)
    %
    % Wait for a subprocess, identified by Pid, to run to completion.
:- pred waitpid(int::in, io.res::out, io::di, io::uo) is det.

    % mkdir(Directory, Mode, Result, !IO)
:- pred mkdir(string::in, io::di, io::uo) is det.

    % chdir(Directory, Res, !IO)
:- pred chdir(string::in, io.res::out, io::di, io::uo) is det.

:- pred exists(string::in, bool::out, io::di, io::uo) is det.

:- implementation.
:- import_module string, int.

:- pragma foreign_decl("C", "
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <spawn.h>
#include <fcntl.h>
").

:-  pragma foreign_proc("C",
    mkdir(Path::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    mkdir(Path, 0755);
").

spawn(Cmd, Args, Result, !IO) :-
    spawn_c(Status, Pid, Cmd, list.length(Args), Args, !IO),
    ( if Status = 0 then
        Result = ok(Pid)
    else
        Result = error(make_io_error("posix_spawnp error code: " ++ string(Status)))
    ).

waitpid(Pid, Result, !IO) :-
    waitpid_c(Pid, Status, !IO),
    ( if Status = 0 then
        Result = ok
    else
        Result = error(make_io_error("waitpid error code: " ++ string(Status)))
    ).

:- pred spawn_c(int::out, int::out,
    string::in, int::in, list(string)::in,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    spawn_c(Status::out, Pid::out,
        Command::in, Len::in, Args::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int pid = Pid;
    int i;
    char **args = malloc(sizeof(char *) * (Len + 2));
    args[0] = Command;
    args[Len + 1] = NULL;
    for (i = 0; i < Len; i++) {
        args[i+1] = (char*) MR_list_head(Args);
        Args = MR_list_tail(Args);
    }
    Status = posix_spawnp(&pid, Command, NULL, NULL, args, environ);
    free(args);
").

:- pred waitpid_c(int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    waitpid_c(Pid::in, Status::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int waitres;
    waitpid(Pid, &waitres, 0);
    if (WIFEXITED(waitres)) {
        Status = WEXITSTATUS(waitres);
    } else {
        Status = -1;
    }
").

chdir(Dir, Result, !IO) :-
    chdir_c(Dir, Status, !IO),
    ( if Status = 0 then
        Result = ok
    else
        Result = error(make_io_error("chdir error code: " ++ string(Status)))
    ).

:- pred chdir_c(string::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    chdir_c(Dir::in, Status::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Status = chdir(Dir);
").

:- pragma foreign_proc("C",
    exists(Path::in, Exists::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int res = open(Path, O_RDONLY);
    if (res == -1) {
        Exists = MR_NO;
    } else {
        Exists = MR_YES;
        close(res);
    }
").

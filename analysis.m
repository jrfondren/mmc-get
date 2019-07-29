:- module analysis.
:- interface.
:- import_module package, hash_table, list.

    % count_tags(Packages, TagTable)
    %
:- pred count_tags(list(package)::in, hash_table(string, int)::hash_table_uo) is det.

    % count_packages(Packages, PackageTable)
    %
:- pred count_packages(list(package)::in, hash_table(string, int)::hash_table_uo) is det.

    % duplicate_packages(Packages, Names)
    %
    % yield list of names that are seen more than once in Packages
    %
:- pred duplicate_packages(list(package)::in, list(string)::out) is det.

:- implementation.
:- import_module int.

count_tags(L, HT) :- count_tags(L, init_default(string_hash), HT).

:- pred count_tags(list(package)::in,
    hash_table(string, int)::hash_table_di,
    hash_table(string, int)::hash_table_uo) is det.
count_tags([], !HT).
count_tags([P | Ps], !HT) :-
    count_taglist(P^tags, !HT),
    count_tags(Ps, !HT).

:- pred count_taglist(list(string)::in,
    hash_table(string, int)::hash_table_di,
    hash_table(string, int)::hash_table_uo) is det.
count_taglist([], !HT).
count_taglist([T | Ts], !HT) :-
    ( if Count = search(!.HT, T) then
        !:HT = det_update(!.HT, T, Count + 1)
    else
        !:HT = det_insert(!.HT, T, 1)
    ),
    count_taglist(Ts, !HT).

count_packages(Ps, HT) :-
    count_packages(Ps, init_default(string_hash), HT).

:- pred count_packages(list(package)::in,
    hash_table(string, int)::hash_table_di,
    hash_table(string, int)::hash_table_uo) is det.
count_packages([], !HT).
count_packages([P | Ps], !HT) :-
    Name = P^name,
    ( if Count = search(!.HT, Name) then
        !:HT = det_update(!.HT, Name, Count + 1)
    else
        !:HT = det_insert(!.HT, Name, 1)
    ),
    count_packages(Ps, !HT).

duplicate_packages(Ps, Dups) :-
    count_packages(Ps, HT),
    fold((pred(Name::in, Count::in, L0::in, L::out) is det :-
        ( if Count > 1 then
            L = [Name | L0]
        else
            L = L0
        )), HT, [], Dups).

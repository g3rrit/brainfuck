
% -------------------- MEMORY --------------------
:- dynamic memory/2.
:- dynamic mem_pos/1.

mem_pos(0).

mem(Y) :-
        mem_pos(X),
        memory(X, Y).

mem(Y) :-
        mem_pos(X),
        Y is 0,
        asserta(memory(X, Y)),!.

mem_set(Y) :-
        mem_pos(X),
        retractall(memory(X, _)),
        asserta(memory(X, Y)).

mem_inc :-
        mem(Y),
        NY is Y + 1,
        mem_set(NY).

mem_dec :-
        mem(Y),
        NY is Y - 1,
        mem_set(NY).

mem_next :-
        mem_pos(X),
        NX is X + 1,
        retractall(mem_pos(_)),
        asserta(mem_pos(NX)),!.

mem_prev :-
        mem_pos(X),
        NX is X - 1,
        retractall(mem_pos(_)),
        asserta(mem_pos(NX)),!.

mem_print :-
        mem_pos(X),
        mem(Y),
        format("mem [~w]: ~d\n", [X, Y]).

% -------------------- TAPE --------------------
:- dynamic tape/2.
:- dynamic tape_pos/1.
:- dynamic tape_max_pos/1.

tape_pos(0).
tape_max_pos(1).

tape_get(Y) :-
        tape_pos(X),
        tape(X, Y).

tape_get(Y) :-
        tape_pos(X),
        X >= 0,
        tape_max_pos(MX),
        X =< MX,
        NMX is MX + 1,
        retractall(tape_max_pos(_)),
        asserta(tape_max_pos(NMX)),
        get(Y),
        asserta(tape(X, Y)),!.

tape_get(_) :-
        format("invalid tape position\n"),
        halt.

tape_next(Y) :-
        tape_pos(X),
        retractall(tape_pos(_)),
        NX is X + 1,
        asserta(tape_pos(NX)),
        tape_get(Y),!.

tape_prev(Y) :-
        tape_pos(X),
        retractall(tape_pos(_)),
        NX is X - 1,
        asserta(tape_pos(NX)),
        tape_get(Y),!.

% -------------------- SEEKING --------------------
seek_right :-
        tape_next(Y),!,
        choose_right(Y).

choose_right(91) :-
        seek_right,
        seek_right.

choose_right(93).

choose_right(_) :-
        seek_right.

seek_left :-
        tape_prev(Y),!,
        choose_left(Y).

choose_left(93) :-
        seek_left,
        seek_left.

choose_left(91).

choose_left(_) :-
        seek_left.

% -------------------- EXECUTING --------------------
exec(62) :- % >
        mem_next.

exec(60) :- % <
        mem_prev.

exec(43) :- % +
        mem_inc.

exec(45) :- % -
        mem_dec.

exec(46) :- % .
        mem(Y),
        put(Y),
        flush_output.

exec(44) :- % ,
        get(C),
        mem_set(C).

exec(91) :- % [
        mem(X),
        (X =:= 0 -> seek_right ; true).

exec(93) :- % ]
        mem(X),
        (X =\= 0 -> seek_left ; true).

exec(-1) :-
        halt.

exec(_).

next :-
        tape_next(Y),
        exec(Y),!,
        next.

% -------------------- ENTRY_POINT --------------------
:-
        next.
        halt.

enable_recursion.

__INCLUDE_HEAD_PRED__

max_body(8).
max_vars(6).

% Declare atomic separation logic with types and directions
body_pred(eq, 2).
type(eq, (node, node)).
direction(eq, (in, in)).

body_pred(neq, 2).
type(neq, (node, node)).
direction(neq, (in, in)).

__INCLUDE_FIELDS__

% Declare set language describing allocated locations.

body_pred(empty, 1).
type(empty, (set, )).
direction (empty, (in, )).

body_pred(allocated, 2).
type(allocated, (node, set)).
direction(allocated, (in, in)).

body_pred(remove, 3).
type(remove, (set, node, set)).
direction(remove, (in, in, out)).

%% ============================================
%% Syntax constraint
%% ============================================

%% Helper definitions

base_case(C) :- clause(C), body_literal(C, empty, _, _).
inductive_case(C) :- clause(C), body_literal(C, allocated, _, _).

%% At least one base and inductive clause

:- #count{ C : base_case(C) } < 1.
:- #count{ C : inductive_case(C) } < 1.


%% Base case syntax restrictions

:- base_case(C), body_literal(C, P, _, _), P != eq, P != neq, P != empty.

%% Inductive case syntax restrictions

%%% Allocation-related predicates appear exactly once
:- inductive_case(R), #count{Vars : body_literal(R, allocated, 2, Vars)} != 1.
:- inductive_case(R), #count{Vars : body_literal(R, remove, 3, Vars)} != 1.

__INCLUDE_FIELD_CONSTRAINTS__

%% Determinism

has_eq_neq_pair(Cb, Ci) :-
    base_case(Cb),
    inductive_case(Ci),
    body_literal(Cb, eq, 2, (X, Y)),
    body_literal(Ci, neq, 2, (X, Y)).

has_eq_neq_pair(Cb, Ci) :-
    base_case(Cb),
    inductive_case(Ci),
    body_literal(Cb, eq, 2, (X, Y)),
    body_literal(Ci, neq, 2, (Y, X)).

:- base_case(B), inductive_case(I), not has_eq_neq_pair(B, I).

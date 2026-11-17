enable_recursion.

__SETTING__

__HEAD_PRED_DEF__

% Pure properties
body_pred(eq, 2).
type(eq, (node, node)).
direction(eq, (in, in)).

body_pred(eq_nil, 1).
type(eq_nil, (node,)).
direction(eq_nil, (in,)).

body_pred(neq, 2).
type(neq, (node, node)).
direction(neq, (in, in)).

body_pred(neq_nil, 1).
type(neq_nil, (node,)).
direction(neq_nil, (in,)).

body_pred(any, 1).
type(any, (node, )).
direction(any, (in,)).

body_pred(nil, 1).
type(nil, (node,)).
direction(nil, (out,)).

__FIELD_DEFS__

%% --------------------------------------------
%% Declaration of body predicates
%% --------------------------------------------

__BODY_PRED_DEFS__

%% ============================================
%% Syntax constraint
%% ============================================

%% Helper definitions

base_case(C) :- clause(C), not body_literal(C, __HEAD_PRED__, _, _).
inductive_case(C) :- clause(C), not base_case(C).

__HELPER_DEFS__
established(R, T) :- head_var(R, T).

%% At least one base and inductive clause

:- #count{ C : base_case(C) } < 1.
:- #count{ C : inductive_case(C) } < 1.

%% Base cases only contain pure constraints.

:- base_case(C), body_literal(C, P, _, _), P != eq, P != neq, P != eq_nil, P != neq_nil, P != any.

%% ----------------------------------------------------------------------------
%% Handling of nil-terminated predicates.
%%
%%   When the definition needs e.g. P(x, nil) where P is an existing predicate,
%%   we need a way to introduce nil variable.
%% ----------------------------------------------------------------------------

%% true if variable was introduced by nil()
nil_var(R, V) :- body_literal(R, nil, 1, (V,)).

%% nil() appears at most once in each rule
:- clause(R), #count{V : body_literal(R, nil, 1, (V,))} > 1.

%% Inductive case syntax restrictions

__FIELD_CONSTRAINTS__

%%% Every target variable must be established
:- target(R, T), not established(R, T).

__PREDICATE_CONSTRAINTS__

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

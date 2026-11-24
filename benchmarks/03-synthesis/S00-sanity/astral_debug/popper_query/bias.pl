enable_recursion.

max_body(6).
max_vars(7).

head_pred(pred1, 3).
type(pred1, (node,node,node)).
direction(pred1, (in,in,in)).

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

body_pred(topXRef_NLS,2).
type(topXRef_NLS, (node, node)).
direction(topXRef_NLS, (in, out)).
body_pred(topXRef_NLS_is_nil, 1).
type(topXRef_NLS_is_nil, (node, )).
direction(topXRef_NLS_is_nil, (in, )).
topXRef_NLS_class(topXRef_NLS).
topXRef_NLS_class(topXRef_NLS_is_nil).
body_pred(downXRef_LS,2).
type(downXRef_LS, (node, node)).
direction(downXRef_LS, (in, out)).
body_pred(downXRef_LS_is_nil, 1).
type(downXRef_LS_is_nil, (node, )).
direction(downXRef_LS_is_nil, (in, )).
downXRef_LS_class(downXRef_LS).
downXRef_LS_class(downXRef_LS_is_nil).
body_pred(nextXRef_LS,2).
type(nextXRef_LS, (node, node)).
direction(nextXRef_LS, (in, out)).
body_pred(nextXRef_LS_is_nil, 1).
type(nextXRef_LS_is_nil, (node, )).
direction(nextXRef_LS_is_nil, (in, )).
nextXRef_LS_class(nextXRef_LS).
nextXRef_LS_class(nextXRef_LS_is_nil).

%% --------------------------------------------
%% Declaration of body predicates
%% --------------------------------------------

body_pred(ls, 2).
type(ls, (node,node)).
direction(ls, (in,in)).

%% ============================================
%% Syntax constraint
%% ============================================

%% Helper definitions

base_case(C) :- clause(C), not body_literal(C, pred1, _, _).
inductive_case(C) :- clause(C), not base_case(C).

root(C, V) :- head_literal(C, _, 3, (V, _, _)).

head_var(C, V) :- head_literal(C, _, 3, (V, _, _)).
head_var(C, V) :- head_literal(C, _, 3, (_, V, _)).
head_var(C, V) :- head_literal(C, _, 3, (_, _, V)).
target(R,T ) :- body_literal(R, topXRef_NLS, 2, (_, T)).
target(R,T ) :- body_literal(R, downXRef_LS, 2, (_, T)).
target(R,T ) :- body_literal(R, nextXRef_LS, 2, (_, T)).
established(R, V) :- body_literal(R, pred1, 3, (V, _, _)).
established(R, V) :- body_literal(R, ls, 2, (V, _)).
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

param_ok(R, V) :- head_var(R, V).
param_ok(R, V) :- nil_var(R, V).

%% Inductive case syntax restrictions

:- inductive_case(R), #count{P, Vars : body_literal(R, P, _, Vars), topXRef_NLS_class(P)} != 1.
:- body_literal(R, topXRef_NLS, 2, (T, _)), not root (R, T).
:- body_literal(R, topXRef_NLS_is_nil, 1, (T,)), not root (R, T).
:- inductive_case(R), #count{P, Vars : body_literal(R, P, _, Vars), downXRef_LS_class(P)} != 1.
:- body_literal(R, downXRef_LS, 2, (T, _)), not root (R, T).
:- body_literal(R, downXRef_LS_is_nil, 1, (T,)), not root (R, T).

%%% Every target variable must be established
:- target(R, T), not established(R, T).

:- body_literal(R, pred1, 3, (V1,V2,V3)), not head_var(R, V1), not head_var(R, V2), not head_var(R, V3).

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

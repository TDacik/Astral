enable_recursion.

max_body(6).
max_vars(7).

head_pred(pred1, 2).
type(pred1, (node,node)).
direction(pred1, (in,in)).

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

% Definitions for field next
body_pred(nextXRef_LS,2).
direction(nextXRef_LS, (in, out)).
type(nextXRef_LS, (node, node)).

body_pred(nextXRef_LS_is_nil, 1).
direction(nextXRef_LS_is_nil, (in, )).
type(nextXRef_LS_is_nil, (node, )).

nextXRef_LS_class(nextXRef_LS).
nextXRef_LS_class(nextXRef_LS_is_nil).

% Definitions for field data
body_pred(dataXBitvector8,2).
direction(dataXBitvector8, (in, out)).
type(dataXBitvector8, (node, data)).

body_pred(dataXBitvector8_is_any, 1).
direction(dataXBitvector8_is_any, (in, )).
type(dataXBitvector8_is_any, (node, )).

dataXBitvector8_class(dataXBitvector8).
dataXBitvector8_class(dataXBitvector8_is_any).

%% --------------------------------------------
%% Declaration of body predicates
%% --------------------------------------------



%% ============================================
%% Syntax constraint
%% ============================================

%% Helper definitions

base_case(C) :- clause(C), not body_literal(C, pred1, _, _).
inductive_case(C) :- clause(C), not base_case(C).

root(C, V) :- head_literal(C, _, 2, (V, _)).

head_var(C, V) :- head_literal(C, _, 2, (V, _)).
head_var(C, V) :- head_literal(C, _, 2, (_, V)).
target(R,T ) :- body_literal(R, nextXRef_LS, 2, (_, T)).
target(R,T ) :- body_literal(R, dataXBitvector8, 2, (_, T)).
established(R, V) :- body_literal(R, pred1, 2, (V, _)).
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

:- inductive_case(R), #count{P, Vars : body_literal(R, P, _, Vars), nextXRef_LS_class(P)} != 1.
:- body_literal(R, nextXRef_LS, 2, (T, _)), not root (R, T).
:- body_literal(R, nextXRef_LS_is_nil, 1, (T,)), not root (R, T).
:- inductive_case(R), #count{P, Vars : body_literal(R, P, _, Vars), dataXBitvector8_class(P)} != 1.
:- body_literal(R, dataXBitvector8, 2, (T, _)), not root (R, T).
:- body_literal(R, dataXBitvector8_is_nil, 1, (T,)), not root (R, T).

%%% Every target variable must be established
:- target(R, T), not established(R, T).

:- body_literal(R, pred1, 2, (V1,V2)), not head_var(R, V1), not head_var(R, V2).

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

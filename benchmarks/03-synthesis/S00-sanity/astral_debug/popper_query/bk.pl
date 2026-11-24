%% ============================================================================
%% Definitions of helper predicates
%% ============================================================================

eq(X,X).
neq(X,Y) :- X \= Y.

nil(X) :- is_null(X).
eq_nil(X) :- is_null(X).
neq_nil(X) :- \+ is_null(X).

any(_).

%% ============================================================================
%% Auxiliary field definitions
%% ============================================================================

topXRef_NLS_is_nil(X) :- topXRef_NLS(X, Y), is_null(Y).
downXRef_LS_is_nil(X) :- downXRef_LS(X, Y), is_null(Y).
nextXRef_LS_is_nil(X) :- nextXRef_LS(X, Y), is_null(Y).

%% ============================================================================
%% Definitions of known inductive predicates
%% ============================================================================

ls(X,Y) :- eq(X, Y).
ls(X,Y) :- neq(X, Y), nextXRef_LS(X, N97), ls(N97,Y).

%% ============================================================================
%% Definitions of heaps used in models in exs.pl
%% ============================================================================

is_null(loc0_1).
topXRef_NLS(loc6_0, loc4_0).
downXRef_LS(loc6_0, loc1_0).
topXRef_NLS(loc5_0, loc6_0).
downXRef_LS(loc5_0, loc3_0).
nextXRef_LS(loc3_0, loc2_0).
nextXRef_LS(loc1_0, loc2_0).
is_null(loc0_0).

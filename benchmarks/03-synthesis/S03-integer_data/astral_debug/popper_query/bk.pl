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

nextXRef_LS_is_nil(X) :- nextXRef_LS(X, Y), is_null(Y).
dataXBitvector8_is_any(_).

%% ============================================================================
%% Definitions of known inductive predicates
%% ============================================================================



%% ============================================================================
%% Definitions of heaps used in models in exs.pl
%% ============================================================================

is_null(loc0_2).
nextXRef_LS(loc2_1, loc0_1).
dataXBitvector8(loc2_1, 127).
nextXRef_LS(loc1_1, loc2_1).
dataXBitvector8(loc1_1, 255).
is_null(loc0_1).
nextXRef_LS(loc3_0, loc0_0).
dataXBitvector8(loc3_0, 0).
nextXRef_LS(loc2_0, loc3_0).
dataXBitvector8(loc2_0, 192).
nextXRef_LS(loc1_0, loc2_0).
dataXBitvector8(loc1_0, 64).
is_null(loc0_0).

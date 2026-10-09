eq(X,X).
neq(X,Y) :- X \= Y.

eq_nil(X) :- is_null(X).
neq_nil(X) :- not is_null(X).

any(_).

%% Auxiliary field definitions
__FIELD_DEFS__

%% Definitions of existing inductive predicates
__PREDICATE_DEFS__

%% Definitions of heaps used to create examples
__HEAP_DEFS__

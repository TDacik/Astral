open Backend_sig

module Init ( ) : sig

  include BACKEND
  include OptimizingSolverBuilder.OPTIMIZING_SOLVER

end

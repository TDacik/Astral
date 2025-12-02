open Backend_sig

val is_available : unit -> bool

module Init ( ) : sig

  include BACKEND

end

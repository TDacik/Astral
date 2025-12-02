
open Logger_sig
open Debug_sig

include DEBUG_OUTPUT

module QueryDir (C : CONFIG_WITH_DIR) : sig
  include LOGGER
  include DEBUG_OUTPUT
end

module SubQueryDir (C : CONFIG_WITH_DIR) : sig
  include LOGGER
  include DEBUG_OUTPUT
end

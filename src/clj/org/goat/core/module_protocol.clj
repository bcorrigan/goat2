(ns org.goat.core.module-protocol
  "Core protocol defining the module abstraction for Goat modules.

  Modules implement this protocol via the defmodule macro.")

(defprotocol Module
  "Protocol for Goat modules.

  Modules don't implement this directly - the defmodule macro generates
  implementations automatically."

  (get-module-name [this]
    "Return the module name as a string")

  (get-commands [this]
    "Return vector of command keywords [:roll :toss :weather]")

  (get-message-type [this]
    "Return message type filter:
     :all       - Receives every message (WANT_ALL_MESSAGES)
     :unclaimed - Receives messages no other module claimed (WANT_UNCLAIMED_MESSAGES)
     :commands  - Only receives matching commands (WANT_COMMAND_MESSAGES)")

  (wants-private? [this]
    "Return true if module wants to receive private messages")

  (process-message [this msg]
    "Process a message. Called by dispatcher.
     This is the main entry point that delegates to
     process-channel-message or process-private-message"))

(defrecord ModuleMetadata
  [module-name    ; String - name of the module
   commands       ; Vector of keywords - [:roll :toss]
   message-type   ; Keyword - :all, :unclaimed, or :commands
   wants-private  ; Boolean - true if module wants private messages
   process-fn]    ; Function (fn [msg] ...) - the actual message processor

  Module
  (get-module-name [this] module-name)
  (get-commands [this] commands)
  (get-message-type [this] message-type)
  (wants-private? [this] wants-private)
  (process-message [this msg] (process-fn msg)))

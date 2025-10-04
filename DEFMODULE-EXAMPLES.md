# defmodule Macro Examples

The `defmodule` macro simplifies creating Goat modules in Clojure with cleaner, more idiomatic syntax.

## Basic Example

```clojure
(ns org.goat.module.MyModule
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]))

(defmodule MyModule
  :commands [:hello :goodbye]

  (defn process-channel-message [m]
    (case (msg/command m)
      :hello (msg/reply m "Hello!")
      :goodbye (msg/reply m "Goodbye!")
      nil)))
```

## Message Reception Options

The `:receive-messages` option controls which messages your module receives:

### `:commands` (default)
Only receive messages with commands in your `:commands` list:
```clojure
(defmodule CommandsOnly
  :commands [:foo :bar]
  :receive-messages :commands  ; optional, this is the default

  (defn process-channel-message [m]
    (case (msg/command m)
      :foo (msg/reply m "Got foo!")
      :bar (msg/reply m "Got bar!")
      nil)))
```

### `:unclaimed`
Receive messages that no other module claimed (useful for fallback handlers):
```clojure
(defmodule Fallback
  :receive-messages :unclaimed

  (defn process-channel-message [m]
    (msg/reply m "I didn't understand that command")))
```

### `:all`
Receive ALL messages (useful for logging, analytics, etc.):
```clojure
(defmodule Logger
  :receive-messages :all

  (defn process-channel-message [m]
    (println "Saw message:" (msg/get-text m))
    ;; Don't reply - just observe
    nil))
```

## Auto-delegation

If you only define `process-channel-message`, the macro automatically creates `process-private-message` that delegates to it (and vice versa):

```clojure
(defmodule AutoDelegate
  :commands [:test]

  ;; Only define one - the other is auto-generated
  (defn process-channel-message [m]
    (msg/reply m "Works in channels AND private messages!")))
```

To handle them differently, define both explicitly:

```clojure
(defmodule DifferentHandlers
  :commands [:test]

  (defn process-channel-message [m]
    (msg/reply m "This is a public response"))

  (defn process-private-message [m]
    (msg/reply m "This is a private response")))
```

## Message Wrapper Benefits

The `msg` parameter is automatically wrapped with a protocol providing idiomatic Clojure functions:

```clojure
;; Instead of Java interop:
(.reply m "text")
(.getModCommand m)
(.getSender m)

;; Use idiomatic Clojure:
(msg/reply m "text")
(msg/command m)        ; returns keyword like :hello
(msg/sender m)
(msg/chat-id m)
(msg/private? m)
```

## Complete Example

Here's CoreCommands as a real-world example:

```clojure
(ns org.goat.module.CoreCommands
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.users :as users])
  (:import [org.goat.util StringUtil]
           [org.goat.core Constants]))

(defmodule CoreCommands
  :commands [:gc :mem :uptime :goat :version :setchat]
  :receive-messages :all

  (defn process-channel-message [m]
    (case (msg/command m)
      :mem (msg/reply m (get-detailed-memory-info))
      :gc (do (System/gc) (msg/reply m "🗑️ Performed garbage collection."))
      :uptime (msg/reply m (get-enhanced-uptime))
      :goat (msg/reply m (str Constants/BOLD "🐐 Goat!" Constants/END_BOLD))
      :version (msg/reply m (get-detailed-version-info))
      :setchat (do
                 (users/user-add (msg/sender m) (msg/chat-id m))
                 (msg/reply m "✅ OK. I've set this chat as your private chat."))
      nil)))
```

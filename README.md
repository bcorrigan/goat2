# Goat Rebirth

Simple framework to bring back goat but on telegram.

Implement "similar" classes for Message, Module etc etc so that there's some API similarity to at least some degree.

Then port some modules.

# For Module Authors

## Writing Modules with `defmodule`

The `defmodule` macro provides an idiomatic Clojure way to create Goat modules with minimal boilerplate.

### Basic Structure

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

### Options

#### `:commands`

A vector of keywords representing the commands this module handles:

```clojure
:commands [:roll :toss :flip]
```

When a user types `goat, roll 3d6` or just `roll 3d6`, modules with `:roll` in their commands will receive the message.

#### `:receive-messages`

Controls which messages your module receives:

- **`:commands`** (default) - Only messages with commands in your `:commands` list
- **`:unclaimed`** - Messages that no other module claimed (useful for fallback handlers)
- **`:all`** - All messages (useful for logging, URL capture, etc.)

Examples:

```clojure
;; Only handle specific commands (default)
(defmodule Calculator
  :commands [:calc :=]
  :receive-messages :commands  ; optional, this is default

  (defn process-channel-message [m] ...))

;; Handle messages no other module wanted
(defmodule HelpFallback
  :receive-messages :unclaimed

  (defn process-channel-message [m]
    (msg/reply m "Unknown command. Try 'help'")))

;; Observe all messages (e.g., for URL capture)
(defmodule URLCapture
  :receive-messages :all

  (defn process-channel-message [m]
    (when-let [url (extract-url (msg/get-text m))]
      (save-url url))
    ;; Don't reply - just observe
    nil))
```

#### `:wants-private`

Controls whether the module receives private messages (default: `true`):

```clojure
(defmodule PublicOnly
  :commands [:announce]
  :wants-private false  ; only works in channels

  (defn process-channel-message [m] ...))
```

### Message Processing Functions

Define one or both of these functions:

#### `process-channel-message`

Called when a message arrives in a channel (group chat):

```clojure
(defn process-channel-message [m]
  (msg/reply m "This is a public response"))
```

#### `process-private-message`

Called when a message arrives as a private (1-on-1) message:

```clojure
(defn process-private-message [m]
  (msg/reply m "This is a private response"))
```

**Auto-delegation:** If you only define one, the other is automatically generated to delegate to it. This means defining just `process-channel-message` makes your module work in both contexts by default.

### Message Wrapper API

The message parameter `m` is automatically wrapped with idiomatic Clojure functions:

#### Basic Operations

```clojure
(msg/reply m "response text")           ; Send a reply
(msg/reply-image m rendered-image)      ; Send an image
```

#### Getting Information

```clojure
(msg/command m)       ; => :roll (as keyword)
(msg/get-text m)      ; => "goat, roll 3d6" (full text)
(msg/mod-text m)      ; => "3d6" (text after command)
(msg/sender m)        ; => "username"
(msg/chat-id m)       ; => 123456
(msg/chatname m)      ; => "My Cool Channel"
(msg/private? m)      ; => true/false
(msg/has-text? m)     ; => true/false
(msg/has-image? m)    ; => true/false
```

### Complete Example: DiceRoll Module

```clojure
(ns org.goat.module.DiceRoll
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.dice :as dice]))

(defmodule DiceRoll
  :commands [:roll :toss]

  (defn process-channel-message [m]
    (case (msg/command m)
      :roll (let [dice-spec (msg/mod-text m)
                  result (dice/roll dice-spec)]
              (msg/reply m (str (msg/sender m) ": " result)))

      :toss (let [result (rand-nth ["Heads" "Tails"])]
              (msg/reply m result))

      nil)))
```

### Complete Example: CoreCommands Module

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
      :mem (msg/reply m (get-memory-info))
      :gc (do (System/gc)
              (msg/reply m "🗑️ Performed garbage collection."))
      :uptime (msg/reply m (get-uptime-info))
      :goat (msg/reply m (str Constants/BOLD "🐐 Goat!" Constants/END_BOLD))
      :version (msg/reply m (get-version-info))
      :setchat (do
                 (users/user-add (msg/sender m) (msg/chat-id m))
                 (msg/reply m "✅ OK. I've set this chat as your private chat."))
      nil)))
```

### Tips

1. **Always handle the nil case** in your `case` statement for commands your module doesn't recognize
2. **Keep modules focused** - one module per feature/responsibility
3. **Use helper functions** for complex logic outside the message handlers
4. **Return nil** when you don't want to send a response (e.g., for passive observation)
5. **Test locally** using CLI mode: `lein run -- -test`

## CLI Test Mode

Test your modules locally without connecting to Telegram:

```bash
lein run -- -test
```

Then type commands interactively:
```
> roll 3d6
TestUser: 3d6:2,5,4:11 Total:11

> toss
Heads.
```

See `CLI-TEST-MODE.md` for more details.

# Wordle Module

A simple wordle-like module which records extensive performance stats and can be played in a channel.

![game](./resources/game.png)

![stats](./resources/wordle-stats.jpg)

(ns org.goat.core.format
  "Platform-specific formatting system.

   Provides formatting tags (bold, underline, etc.) based on platform type.
   Telegram uses HTML tags, CLI uses plain text, future platforms can add their own.

   Usage:
   (let [f (formatter :telegram)]
     (str (bold f \"Hello\") \" world\"))
   => \"<b>Hello</b> world\"")

;; =============================================================================
;; Multimethod Dispatch
;; =============================================================================

(defmulti formatter
  "Return formatting map for a given platform type keyword.

   Dispatches on :telegram, :cli, etc.

   Returns a map with keys:
   - :bold, :end-bold - Bold text
   - :underline, :end-underline - Underlined text
   - :pre, :end-pre - Preformatted/code block text
   - :normal - Reset to normal formatting
   - :reverse - Reverse video (if supported)

   Example:
   (formatter :telegram)
   => {:bold \"<b>\" :end-bold \"</b>\" ...}"
  identity)

;; =============================================================================
;; Platform Formatters
;; =============================================================================

(defmethod formatter :telegram [_]
  "Telegram HTML formatting tags.

   Telegram supports HTML parse mode with basic tags."
  {:bold "<b>"
   :end-bold "</b>"
   :underline "<u>"
   :end-underline "</u>"
   :pre "<pre>"
   :end-pre "</pre>"
   :normal ""
   :reverse ""})

(defmethod formatter :cli [_]
  "CLI plain text formatter (no formatting).

   Returns empty strings for all format tags, resulting in plain text output.
   Future enhancement: Could use ANSI escape codes for terminal formatting."
  {:bold ""
   :end-bold ""
   :underline ""
   :end-underline ""
   :pre ""
   :end-pre ""
   :normal ""
   :reverse ""})

(defmethod formatter :default [platform-type]
  "Default formatter for unknown platforms.

   Returns plain text (no formatting) and logs a warning.
   This provides graceful degradation when a new platform is added
   but formatters haven't been updated yet."
  (println "WARN: Unknown platform type" platform-type ", using plain text formatter")
  {:bold ""
   :end-bold ""
   :underline ""
   :end-underline ""
   :pre ""
   :end-pre ""
   :normal ""
   :reverse ""})

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn bold
  "Wrap text in bold formatting.

   Example:
   (bold (formatter :telegram) \"Hello\")
   => \"<b>Hello</b>\""
  [fmt text]
  (str (:bold fmt) text (:end-bold fmt)))

(defn underline
  "Wrap text in underline formatting.

   Example:
   (underline (formatter :telegram) \"Hello\")
   => \"<u>Hello</u>\""
  [fmt text]
  (str (:underline fmt) text (:end-underline fmt)))

(defn pre
  "Wrap text in preformatted/code block formatting.

   Example:
   (pre (formatter :telegram) \"code\")
   => \"<pre>code</pre>\""
  [fmt text]
  (str (:pre fmt) text (:end-pre fmt)))

(defn bold-underline
  "Wrap text in both bold and underline formatting.

   Example:
   (bold-underline (formatter :telegram) \"Hello\")
   => \"<b><u>Hello</u></b>\""
  [fmt text]
  (str (:bold fmt) (:underline fmt) text
       (:end-underline fmt) (:end-bold fmt)))

(defn get-formatter
  "Get formatter for a platform type keyword.

   Convenience function that calls the multimethod.

   Example:
   (get-formatter :telegram)
   => {:bold \"<b>\" :end-bold \"</b>\" ...}"
  [platform-type]
  (formatter platform-type))

;; =============================================================================
;; Examples
;; =============================================================================

(comment
  ;; Get formatters
  (formatter :telegram)
  ;; => {:bold "<b>" :end-bold "</b>" ...}

  (formatter :cli)
  ;; => {:bold "" :end-bold "" ...}

  (formatter :slack)  ; Unknown platform
  ;; WARN: Unknown platform type :slack , using plain text formatter
  ;; => {:bold "" :end-bold "" ...}

  ;; Use helper functions
  (let [f (formatter :telegram)]
    (bold f "Hello"))
  ;; => "<b>Hello</b>"

  (let [f (formatter :telegram)]
    (str (bold f "Username") " rolled " (bold f "6")))
  ;; => "<b>Username</b> rolled <b>6</b>"

  (let [f (formatter :cli)]
    (bold f "Hello"))
  ;; => "Hello"
  )

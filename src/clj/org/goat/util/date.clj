(ns org.goat.util.date
  "Generic date parsing utilities using Hawking ML library.
   Configured for UK date formats (DD/MM/YYYY)."
  (:require [clojure.string :as str])
  (:import [com.zoho.hawking HawkingTimeParser]
           [com.zoho.hawking.datetimeparser.configuration HawkingConfiguration]
           [com.zoho.hawking.language.english.model DatesFound ParserOutput]
           [java.util Date]))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn- create-uk-hawking-config
  "Create a HawkingConfiguration configured for UK date formats.
   - Date format: DD/MM/YYYY
   - Timezone: Europe/London
   - Fiscal year: April to March"
  []
  (let [config (HawkingConfiguration.)]
    (doto config
      (.setFiscalYearStart 4)
      (.setFiscalYearEnd 3)
      (.setTimeZone "Europe/London")
      (.setDateFormat "DD/MM/YYYY"))
    config))

(def ^:private uk-config
  "Shared UK-configured HawkingConfiguration instance"
  (create-uk-hawking-config))

(def ^:private parser
  "Shared HawkingTimeParser instance"
  (HawkingTimeParser.))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn- expand-two-digit-year
  "Convert 2-digit year to 4-digit year.
   Years 00-49 become 2000-2049, years 50-99 become 1950-1999."
  [year-str]
  (let [year (Integer/parseInt year-str)]
    (if (< year 50)
      (+ 2000 year)
      (+ 1900 year))))

(defn- preprocess-date-text
  "Preprocess date text to help Hawking parse it better.
   - Converts 2-digit years to 4-digit years (3/5/26 -> 3/5/2026)
   - Adds context words to bare dates to improve recognition."
  [text]
  (let [text (str/trim text)
        ;; First, expand 2-digit years to 4-digit years
        text-with-year (if-let [match (re-matches #"(\d{1,2}/\d{1,2}/)(\d{2})$" text)]
                         (str (nth match 1) (expand-two-digit-year (nth match 2)))
                         text)]
    ;; If text looks like ONLY a bare date (DD/MM/YYYY), add 'on ' prefix
    (if (re-matches #"^\d{1,2}/\d{1,2}/\d{4}$" text-with-year)
      (str "on " text-with-year)
      text-with-year)))

;; ============================================================================
;; Public API
;; ============================================================================

(defn parse-date
  "Parse a date string using Hawking ML library with UK date format (DD/MM/YYYY).

   Examples:
   - '5/5/2025' -> Date for 5th May 2025
   - 'tomorrow' -> Date for tomorrow
   - 'next week' -> Date for next week
   - '25/12/2025' -> Date for 25th December 2025

   Parameters:
   - text: The text containing a date to parse
   - reference-date: Optional Date to use as reference (defaults to now)

   Returns:
   - A Date object for the parsed date, or nil if no valid date found

   Options:
   - :future-only? - If true, only return dates in the future (default: false)

   Note: Works best with 4-digit years (DD/MM/YYYY). 2-digit years may not parse correctly."
  ([text]
   (parse-date text (Date.)))
  ([text reference-date]
   (parse-date text reference-date {}))
  ([text reference-date {:keys [future-only?] :or {future-only? false}}]
   (when text
     (try
       (let [preprocessed-text (preprocess-date-text text)
             dates-found (.parse parser preprocessed-text reference-date uk-config "eng")
             outputs (.getParserOutputs dates-found)]
         (when (seq outputs)
           ;; Filter out null/invalid dates and apply future-only filter
           (let [valid-outputs (->> outputs
                                    (filter (fn [^ParserOutput po]
                                              (try
                                                (let [start (.. po getDateRange getStart)]
                                                  (and start
                                                       (or (not future-only?)
                                                           (>= (.getMillis start)
                                                               (.getTime reference-date)))))
                                                (catch Exception _ false)))))]
             (when (seq valid-outputs)
               ;; Return the nearest/first valid date
               (let [^ParserOutput nearest (first (sort-by
                                                    (fn [^ParserOutput po]
                                                      (.. po getDateRange getStart getMillis))
                                                    valid-outputs))
                     millis (.. nearest getDateRange getStart getMillis)]
                 (Date. millis))))))
       (catch Exception e
         nil)))))

(defn parse-date-timestamp
  "Like parse-date, but returns a timestamp (milliseconds since epoch) instead of a Date object.
   Returns nil if no valid date found."
  ([text]
   (parse-date-timestamp text (Date.)))
  ([text reference-date]
   (parse-date-timestamp text reference-date {}))
  ([text reference-date options]
   (when-let [date (parse-date text reference-date options)]
     (.getTime date))))

(defn parse-all-dates
  "Parse all dates found in the text, returning a sequence of Date objects.
   Unlike parse-date which returns only the first/nearest date, this returns all dates found."
  ([text]
   (parse-all-dates text (Date.)))
  ([text reference-date]
   (when text
     (try
       (let [dates-found (.parse parser text reference-date uk-config "eng")
             outputs (.getParserOutputs dates-found)]
         (->> outputs
              (keep (fn [^ParserOutput po]
                      (try
                        (let [start (.. po getDateRange getStart)]
                          (when start
                            (Date. (.getMillis start))))
                        (catch Exception _ nil))))))
       (catch Exception e
         [])))))

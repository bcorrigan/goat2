(ns org.goat.module.Weather
  (:require [org.goat.core.macros :refer [defmodule]]
            [org.goat.core.message :as msg]
            [org.goat.db.users :as users]
            [org.httpkit.client :as http]
            [clojure.string :as str])
  (:import [org.goat.core Constants]
           [org.goat.suntimes SunTimes Time SunTimesException]
           [org.goat.util PhaseOfMoon]
           [java.util Date TimeZone GregorianCalendar Calendar]
           [java.text SimpleDateFormat]))

;; ============================================================================
;; Constants
;; ============================================================================

(def codes-url "http://aviationweather.gov/static/adds/metars/stations.txt")

;; Users is a static class - no need to hold a reference

;; ============================================================================
;; Regex Patterns
;; ============================================================================

(def wind-pattern
  #"Wind:\s*from the ([NSEW]+) \(.*\) at (\d+) MPH \(\d+ KT\)(?: gusting to (\d+) MPH \(\d+ KT\))*.*")

(def coordinates-pattern
  #".* (\d+)-(\d+)(?:-\d+)*([NS]) (\d+)-(\d+)(?:-\d+)*([EW]).*")

(def sky-conditions-pattern
  #"Sky conditions:\s*(.*)")

(def weather-pattern
  #"Weather:\s*(.*)")

(def time-pattern
  #".* ([A-Z]{3}) / ((\d+)\.(\d+)\.(\d+) (\d{2})(\d{2}) UTC).*")

;; NOAA sends plain text (no HTML tags), we add the bold formatting
(def temperature-pattern
  #"Temperature:\s*([\d.+-]+)\s*F\s*\(([\d.+-]+)\s*C\)")

(def dew-point-pattern
  #"Dew Point:\s*([\d.+-]+)\s*F\s*\(([\d.+-]+)\s*C\)")

(def precipitation-pattern
  #"Precipitation last hour:\s*(.*)")

(def humidity-pattern
  #"Relative Humidity:\s*(.*)")

;; ============================================================================
;; HTTP Functions
;; ============================================================================

(defn fetch-metar
  "Fetch raw METAR data from NOAA. Returns {:success body} or {:error reason}."
  [station]
  (let [station-upper (str/upper-case station)
        url (str "https://tgftp.nws.noaa.gov/data/observations/metar/stations/"
                 station-upper ".TXT")]
    (try
      (let [{:keys [status body error]} @(http/get url {:timeout 5000})]
        (cond
          error {:error :timeout}
          (= status 404) {:error :invalid-station}
          (= status 200) {:success (str/trim body)}
          :else {:error (str "http-" status)}))
      (catch Exception e
        {:error :exception}))))

(defn fetch-decoded-weather
  "Fetch decoded weather HTML from NOAA. Returns {:success body} or {:error reason}."
  [station]
  (let [station-upper (str/upper-case station)
        url (str "https://tgftp.nws.noaa.gov/data/observations/metar/decoded/"
                 station-upper ".TXT")]
    (try
      (let [{:keys [status body error]} @(http/get url {:timeout 5000})]
        (cond
          error {:error :timeout}
          (= status 404) {:error :invalid-station}
          (= status 200) {:success body}
          :else {:error (str "http-" status)}))
      (catch Exception e
        {:error :exception}))))

;; ============================================================================
;; Parsing Functions
;; ============================================================================

(defn extract-wind
  "Extract wind information from a line."
  [line]
  (when-let [m (re-matches wind-pattern line)]
    {:wind-direction (nth m 1)
     :wind-mph (nth m 2)
     :wind-gust (when (nth m 3) (nth m 3))}))

(defn extract-coordinates
  "Extract latitude and longitude from coordinates line."
  [line]
  (when-let [m (re-matches coordinates-pattern line)]
    (let [lat-deg (Double/parseDouble (nth m 1))
          lat-min (Double/parseDouble (nth m 2))
          lat-dir (nth m 3)
          lon-deg (Double/parseDouble (nth m 4))
          lon-min (Double/parseDouble (nth m 5))
          lon-dir (nth m 6)
          latitude (+ lat-deg (/ lat-min 60.0))
          longitude (+ lon-deg (/ lon-min 60.0))]
      {:latitude (if (= lat-dir "S") (- latitude) latitude)
       :longitude (if (= lon-dir "W") (- longitude) longitude)})))

(defn parse-time
  "Parse report time from HTML."
  [line]
  (when-let [m (re-matches time-pattern line)]
    (try
      (let [year (Integer/parseInt (nth m 3))
            month (Integer/parseInt (nth m 4))
            day (Integer/parseInt (nth m 5))
            hour (Integer/parseInt (nth m 6))
            minute (Integer/parseInt (nth m 7))
            sdf (SimpleDateFormat. "yyyy.MM.dd HHmm zzz")
            report-date (.parse sdf (nth m 2))
            now (Date.)
            minutes-since (/ (- (.getTime now) (.getTime report-date)) 1000 60)]
        {:year year :month month :day day :hour hour :minute minute
         :date report-date :minutes-ago minutes-since})
      (catch Exception e
        nil))))

(defn parse-weather-response
  "Parse NOAA HTML response into weather data map with processed lines."
  [html station]
  (let [lines (str/split-lines html)
        data (atom {:station station
                    :temperature-f ""
                    :temperature-c ""
                    :dew-point-c ""
                    :wind-direction ""
                    :wind-mph ""
                    :wind-gust ""
                    :sky-conditions ""
                    :weather-type ""
                    :precipitation "none"
                    :humidity ""
                    :latitude 0.0
                    :longitude 0.0
                    :report-year 0
                    :report-month 0
                    :report-day 0
                    :report-hour 0
                    :report-minute 0
                    :minutes-ago 0
                    :processed-lines []})
        processed (atom [])]
    (doseq [line lines]
      ;; Skip certain lines
      (when-not (or (str/starts-with? line "ob")
                    (str/starts-with? line "cycle")
                    (str/includes? line "Windchill"))
        (let [;; Process line for temperature conversion, cleanup, and bolding
              processed-line (cond
                               ;; Convert temperature to Celsius only with bold
                               (re-matches temperature-pattern line)
                               (let [m (re-matches temperature-pattern line)]
                                 (str "<b>Temperature:</b> " (nth m 2) " C"))

                               ;; Convert dew point to Celsius only with bold
                               (re-matches dew-point-pattern line)
                               (let [m (re-matches dew-point-pattern line)]
                                 (str "<b>Dew Point:</b> " (nth m 2) " C"))

                               ;; Add bold tags to other field lines (Field: value format)
                               (and (str/includes? line ": ")
                                    (re-matches #"[A-Z].*" (str (first line))))
                               (let [cleaned (str/replace line ":0" "")
                                     [field-name value] (str/split cleaned #": " 2)]
                                 (str "<b>" field-name ":</b> " value))

                               ;; Pass through other lines as-is
                               :else
                               line)]

          ;; Add processed line to output
          (swap! processed conj processed-line)

          ;; Extract data for calculations
          (when-let [wind (extract-wind line)]
            (swap! data merge wind))
          (when-let [coords (extract-coordinates line)]
            (swap! data merge coords))
          (when-let [m (re-matches sky-conditions-pattern line)]
            (swap! data assoc :sky-conditions (nth m 1)))
          (when-let [m (re-matches weather-pattern line)]
            (swap! data assoc :weather-type (nth m 1)))
          (when-let [time-data (parse-time line)]
            (swap! data merge {:report-year (:year time-data)
                               :report-month (:month time-data)
                               :report-day (:day time-data)
                               :report-hour (:hour time-data)
                               :report-minute (:minute time-data)
                               :minutes-ago (:minutes-ago time-data)}))
          (when-let [m (re-matches temperature-pattern line)]
            (swap! data assoc :temperature-f (nth m 1) :temperature-c (nth m 2)))
          (when-let [m (re-matches dew-point-pattern line)]
            (swap! data assoc :dew-point-c (nth m 2)))
          (when-let [m (re-matches precipitation-pattern line)]
            (swap! data assoc :precipitation (nth m 1)))
          (when-let [m (re-matches humidity-pattern line)]
            (swap! data assoc :humidity (nth m 1))))))

    (swap! data assoc :processed-lines @processed)
    @data))

;; ============================================================================
;; Calculation Functions
;; ============================================================================

(defn f-to-c
  "Convert Fahrenheit to Celsius."
  [f]
  (* (- f 32) 5/9))

(defn windchill
  "Calculate windchill given temperature in Fahrenheit and wind speed in mph."
  [temp-f wind-mph]
  (if (and (<= temp-f 50.0) (> wind-mph 3.0))
    (+ 35.74
       (* 0.6215 temp-f)
       (- (* 35.75 (Math/pow wind-mph 0.16)))
       (* 0.4275 temp-f (Math/pow wind-mph 0.16)))
    temp-f))

(defn parse-double-safe
  "Safely parse a double, returning 0.0 on failure."
  [s]
  (try
    (Double/parseDouble s)
    (catch Exception _ 0.0)))

(defn calculate-sun-hours
  "Calculate hours of sunlight."
  [sunrise sunset]
  (if (and sunrise sunset)
    (let [sunrise-hours (.getFractionalHours sunrise)
          sunset-hours (.getFractionalHours sunset)]
      (if (> sunset-hours sunrise-hours)
        (- sunset-hours sunrise-hours)
        (+ sunset-hours (- 24 sunrise-hours))))
    12.0))

(defn calculate-weather-bonus
  "Calculate bonus points for interesting weather conditions."
  [sky-conditions weather-type]
  (let [bonus (atom 0.0)
        sky (or sky-conditions "")
        weather (or weather-type "")]
    ;; Sky conditions bonuses
    (when (str/includes? sky "overcast") (swap! bonus + 5))
    (when (str/includes? sky "partly cloudy") (swap! bonus + 1.5))
    (when (str/includes? sky "mostly cloudy") (swap! bonus + 3))
    (when (str/includes? sky "cloudy") (swap! bonus + 5))

    ;; Weather type bonuses
    (cond
      (str/includes? weather "heavy rain") (swap! bonus + 15)
      (str/includes? weather "light rain") (swap! bonus + 5)
      (str/includes? weather "rain") (swap! bonus + 10))

    (when (str/includes? weather "haze") (swap! bonus + 5))
    (when (str/includes? weather "drizzle") (swap! bonus + 5))
    (when (str/includes? weather "freezing drizzle") (swap! bonus + 10))
    (when (str/includes? weather "ice pellets") (swap! bonus + 15))
    (when (str/includes? weather "blowing dust") (swap! bonus + 10))

    (cond
      (str/includes? weather "heavy fog") (swap! bonus + 15)
      (str/includes? weather "ice fog") (swap! bonus + 20)
      (str/includes? weather "ground fog") (swap! bonus + 15)
      (str/includes? weather "light fog") (swap! bonus + 5)
      (str/includes? weather "fog") (swap! bonus + 10))

    (when (str/includes? weather "freezing spray") (swap! bonus + 20))
    (when (str/includes? weather "freezing") (swap! bonus + 15))
    (when (str/includes? weather "tornado") (swap! bonus + 100))
    (when (str/includes? weather "volcanic ash") (swap! bonus + 100))
    (when (str/includes? weather "water spouts") (swap! bonus + 50))
    (when (str/includes? weather "blowing sand") (swap! bonus + 50))
    (when (str/includes? weather "frost") (swap! bonus + 15))
    (when (str/includes? weather "lightning") (swap! bonus + 30))
    (when (str/includes? weather "thunder") (swap! bonus + 30))

    (cond
      (str/includes? weather "heavy ice pellets") (swap! bonus + 20)
      (str/includes? weather "light ice pellets") (swap! bonus + 10)
      (str/includes? weather "ice pellets") (swap! bonus + 15))

    (cond
      (str/includes? weather "heavy ice crystals") (swap! bonus + 30)
      (str/includes? weather "light ice crystals") (swap! bonus + 20)
      (str/includes? weather "ice crystals") (swap! bonus + 25))

    (cond
      (str/includes? weather "heavy sleet") (swap! bonus + 20)
      (str/includes? weather "light sleet") (swap! bonus + 10)
      (str/includes? weather "sleet") (swap! bonus + 15))

    (cond
      (str/includes? weather "heavy snow") (swap! bonus + 25)
      (str/includes? weather "light snow") (swap! bonus + 15)
      (str/includes? weather "snow") (swap! bonus + 20))

    (when (str/includes? weather "smoke") (swap! bonus + 75))
    (when (str/includes? weather "towering") (swap! bonus + 7.5))

    @bonus))

(defn calculate-score
  "Calculate weather interestingness score."
  [weather sunrise sunset]
  (let [{:keys [wind-mph wind-gust temperature-c sky-conditions
                weather-type humidity]} weather
        wind (parse-double-safe (or wind-mph "0"))
        gust (parse-double-safe (or wind-gust "0"))
        temp (parse-double-safe (or temperature-c "15"))
        humid-str (or humidity "50%")
        humid-num (try
                    (Integer/parseInt (str/replace humid-str #"%" ""))
                    (catch Exception _ 50))
        humid (let [h (double humid-num)]
                (if (< h 50)
                  (+ h (* 2 (- 50 h)))
                  h))
        humid-normalized (- (/ humid 100.0) 0.5)
        sun-hours (calculate-sun-hours sunrise sunset)
        bonus (calculate-weather-bonus sky-conditions weather-type)]
    (+ (/ wind 2.0)
       (Math/abs (- 15.0 temp))
       (/ (- gust wind) 3.0)
       (* humid-normalized (Math/abs temp) 0.5)
       (Math/abs (- 12.0 sun-hours))
       bonus)))

(defn is-daylight?
  "Determine if it's currently daytime."
  [report-hour report-minute sunrise sunset]
  (if (and sunrise sunset)
    (let [fractional-hour (+ report-hour (/ report-minute 60.0))
          sunrise-hours (.getFractionalHours sunrise)
          sunset-hours (.getFractionalHours sunset)]
      (if (< sunrise-hours sunset-hours)
        (and (< sunrise-hours fractional-hour) (> sunset-hours fractional-hour))
        (or (< sunrise-hours fractional-hour) (> sunset-hours fractional-hour))))
    true))

;; ============================================================================
;; Astronomical Functions (Java Interop)
;; ============================================================================

(defn get-sunrise
  "Get sunrise time using SunTimes library."
  [year month day longitude latitude]
  (try
    (SunTimes/getSunriseTimeUTC year month day longitude latitude SunTimes/ZENITH)
    (catch SunTimesException e
      nil)))

(defn get-sunset
  "Get sunset time using SunTimes library."
  [year month day longitude latitude]
  (try
    (SunTimes/getSunsetTimeUTC year month day longitude latitude SunTimes/ZENITH)
    (catch SunTimesException e
      nil)))

(defn format-sun-time
  "Format sun time to local time string."
  [time-obj timezone]
  (if (nil? time-obj)
    ""
    (let [cal (GregorianCalendar. (TimeZone/getTimeZone "UTC"))]
      (.set cal Calendar/HOUR_OF_DAY (.getHour time-obj))
      (.set cal Calendar/MINUTE (.getMinute time-obj))
      (if timezone
        (let [temp-millis (.getTimeInMillis cal)
              local-cal (GregorianCalendar. timezone)]
          (.setTimeInMillis local-cal temp-millis)
          (let [hour (.get local-cal Calendar/HOUR)
                minute (.get local-cal Calendar/MINUTE)
                am-pm (.get local-cal Calendar/AM_PM)]
            (str hour ":"
                 (if (< minute 10) (str "0" minute) minute)
                 (if (= am-pm Calendar/AM) "am" "pm"))))
        (let [hour (.get cal Calendar/HOUR)
              minute (.get cal Calendar/MINUTE)
              am-pm (.get cal Calendar/AM_PM)]
          (str hour ":"
               (if (< minute 10) (str "0" minute) minute)
               (if (= am-pm Calendar/AM) "am" "pm")
               " GMT"))))))

(defn moon-phase
  "Get current moon phase."
  []
  (PhaseOfMoon/phaseAsString (.getTime (Date.))))

(defn moon-phase-short
  "Get current moon phase (short form)."
  []
  (PhaseOfMoon/phaseAsShortString (.getTime (Date.))))

;; ============================================================================
;; Emoji Helper Functions
;; ============================================================================

(defn get-weather-emoji
  "Get emoji for weather type based on conditions."
  [weather-str]
  (let [weather (str/lower-case (or weather-str ""))]
    (cond
      ;; Severe weather
      (str/includes? weather "tornado") "🌪️"
      (str/includes? weather "funnel") "🌪️"

      ;; Thunderstorms
      (or (str/includes? weather "thunderstorm")
          (str/includes? weather "lightning")) "⛈️"

      ;; Snow
      (str/includes? weather "blizzard") "🌨️❄️"
      (str/includes? weather "heavy snow") "❄️❄️"
      (str/includes? weather "snow") "🌨️"
      (str/includes? weather "flurries") "🌨️"

      ;; Ice
      (str/includes? weather "hail") "🧊"
      (str/includes? weather "ice pellets") "🧊"
      (str/includes? weather "freezing") "🧊"

      ;; Rain
      (str/includes? weather "heavy rain") "🌧️🌧️"
      (str/includes? weather "rain") "🌧️"
      (str/includes? weather "drizzle") "🌦️"
      (str/includes? weather "showers") "🌧️"

      ;; Obscuration
      (str/includes? weather "fog") "🌫️"
      (str/includes? weather "mist") "🌫️"
      (str/includes? weather "haze") "🌫️"
      (str/includes? weather "smoke") "🌫️"

      ;; Wind
      (str/includes? weather "squall") "💨"

      ;; Default
      :else "")))

(defn get-sky-emoji
  "Get emoji for sky conditions based on conditions and time of day."
  [sky-str daylight?]
  (let [sky (str/lower-case (or sky-str ""))]
    (cond
      ;; Clear/Sunny
      (or (str/includes? sky "sunny")
          (str/includes? sky "clear")) (if daylight? "☀️" "🌙")

      ;; Moony (our special case for clear at night)
      (str/includes? sky "moony") "🌙"

      ;; Partly cloudy
      (str/includes? sky "partly cloudy") "⛅"
      (str/includes? sky "few") "⛅"
      (str/includes? sky "scattered") "⛅"

      ;; Mostly cloudy
      (or (str/includes? sky "mostly cloudy")
          (str/includes? sky "broken")
          (str/includes? sky "overcast")) "☁️"

      ;; Cloudy
      (str/includes? sky "cloudy") "☁️"

      ;; Default
      :else "☁️")))

(defn add-emoji-to-line
  "Add appropriate emoji to a weather data line."
  [line daylight?]
  (cond
    ;; Location/Station - add at start
    (str/starts-with? line "<b>Location:")
    (str "📍 " line)

    ;; Temperature
    (str/includes? line "<b>Temperature:")
    (str "🌡️ " line)

    ;; Dew Point
    (str/includes? line "<b>Dew Point:")
    (str "💧 " line)

    ;; Wind - add at start
    (str/includes? line "<b>Wind:")
    (str "💨 " line)

    ;; Visibility
    (str/includes? line "<b>Visibility:")
    (str "👁️ " line)

    ;; Sky conditions - dynamic emoji based on conditions
    (str/includes? line "<b>Sky conditions:")
    (let [sky-emoji (get-sky-emoji line daylight?)]
      (str sky-emoji " " line))

    ;; Weather - dynamic emoji based on weather type
    (str/includes? line "<b>Weather:")
    (let [weather-emoji (get-weather-emoji line)]
      (if (str/blank? weather-emoji)
        (str "🌦️ " line)  ; Default weather emoji
        (str weather-emoji " " line)))

    ;; Humidity
    (str/includes? line "<b>Relative Humidity:")
    (str "💧 " line)

    ;; Pressure
    (str/includes? line "<b>Pressure")
    (str "🎚️ " line)

    ;; Default - return as-is
    :else line))

;; ============================================================================
;; Formatting Functions
;; ============================================================================

(defn format-raw-metar
  "Format raw METAR response."
  [metar-text username]
  (str metar-text))

(defn format-weather-report
  "Format complete weather report with all NOAA data plus enhancements."
  [weather-data username]
  (let [{:keys [station temperature-f temperature-c dew-point-c wind-direction
                wind-mph wind-gust sky-conditions weather-type humidity
                latitude longitude report-year report-month report-day
                report-hour report-minute minutes-ago processed-lines]} weather-data

        sunrise (get-sunrise report-year report-month report-day longitude latitude)
        sunset (get-sunset report-year report-month report-day longitude latitude)

        user-tz (when-let [tz-str (users/get-timezone username)]
                  (when-not (str/blank? tz-str)
                    (TimeZone/getTimeZone tz-str)))

        sunrise-str (format-sun-time sunrise user-tz)
        sunset-str (format-sun-time sunset user-tz)

        daylight (is-daylight? report-hour report-minute sunrise sunset)

        ;; Process lines to replace "clear" with "sunny" or "moony" and add emojis
        enhanced-lines (map (fn [line]
                              (let [;; Replace clear with sunny/moony
                                    clear-replaced (if (and (str/includes? line "Sky conditions:")
                                                            (str/includes? line "clear"))
                                                     (if daylight
                                                       (str/replace line "clear" "sunny")
                                                       (str/replace line "clear" "moony"))
                                                     line)]
                                ;; Add emoji to line
                                (add-emoji-to-line clear-replaced daylight)))
                            processed-lines)

        score (calculate-score weather-data sunrise sunset)
        score-rounded (/ (Math/round (* score 100)) 100.0)

        temp-f-num (parse-double-safe temperature-f)
        wind-mph-num (parse-double-safe wind-mph)
        windchill-val (windchill temp-f-num wind-mph-num)

        moon (moon-phase)]

    ;; Build response from NOAA data plus enhancements
    (str (str/join "\n" enhanced-lines) "\n"
         ;; Add windchill if applicable (when different from actual temp and conditions apply)
         (when (and (> temp-f-num 0) (> wind-mph-num 0)
                    (<= temp-f-num 50.0) (> wind-mph-num 3.0)
                    (not= windchill-val temp-f-num))
           (str "🥶 " Constants/BOLD "Windchill:" Constants/END_BOLD " "
                (format "%.1fC" (f-to-c windchill-val)) "\n"))
         ;; Add sunrise/sunset if available
         (when-not (or (str/blank? sunrise-str) (str/blank? sunset-str))
           (str "🌅 " Constants/BOLD "Sunrise:" Constants/END_BOLD " " sunrise-str "\n"
                "🌇 " Constants/BOLD "Sunset:" Constants/END_BOLD " " sunset-str "\n"))
         ;; Add moon phase
         "🌙 " Constants/BOLD "Moon:" Constants/END_BOLD " " moon "\n"
         ;; Add report age if available
         (when (> minutes-ago 0)
           (str "⏰ Reported " (long minutes-ago) " minutes ago.\n"))
         ;; Add score
         "📊 " Constants/BOLD "Score: " Constants/END_BOLD score-rounded)))

;; ============================================================================
;; Command Handler
;; ============================================================================

(defn handle-weather
  "Handle weather command."
  [m]
  (let [text (msg/mod-text m)
        username (msg/sender m)
        command (name (msg/command m))
        is-raw (re-matches #".*(?:raw|metar).*" command)]

    (cond
      ;; Command with no arguments - use saved station
      (str/blank? text)
      (let [station (users/get-weather-station username)]
        (if (str/blank? station)
            (msg/reply m (str "I don't know where you are, " username
                              ", perhaps you should tell me by looking at "
                              Constants/BOLD " " codes-url " " Constants/END_BOLD
                              " and telling me where you are."))
            (if is-raw
              (let [result (fetch-metar station)]
                (if (:success result)
                  (msg/reply m (format-raw-metar (:success result) username))
                  (msg/reply m (cond
                                 (= (:error result) :timeout)
                                 (str "I got bored waiting for the METAR for " station)
                                 (= (:error result) :invalid-station)
                                 (str "That doesn't seem to be a valid location, " username ", sorry. See " codes-url)
                                 :else
                                 (str "Hmmmn. " username ", the NOAA weather server is giving me an error, sorry.")))))
              (let [result (fetch-decoded-weather station)]
                (if (:success result)
                  (let [weather-data (parse-weather-response (:success result) station)]
                    (msg/reply m (format-weather-report weather-data username)))
                  (msg/reply m (cond
                                 (= (:error result) :timeout)
                                 (str "I got bored waiting for the weather report for " station)
                                 (= (:error result) :invalid-station)
                                 (str "That doesn't seem to be a valid location, " username ", sorry. See " codes-url)
                                 :else
                                 (str "Hmmmn. " username ", the NOAA weather server is giving me an error, sorry."))))))))

      ;; Command with 4-letter station code
      (re-matches #"\s*[a-zA-Z0-9]{4}\s*" text)
      (let [station (str/upper-case (str/trim text))]
        (if is-raw
          (let [result (fetch-metar station)]
            (if (:success result)
              (msg/reply m (format-raw-metar (:success result) username))
              (msg/reply m (cond
                             (= (:error result) :timeout)
                             (str "I got bored waiting for the METAR for " station)
                             (= (:error result) :invalid-station)
                             (str "That doesn't seem to be a valid location, " username ", sorry. See " codes-url)
                             :else
                             (str "Hmmmn. " username ", the NOAA weather server is giving me an error, sorry.")))))
          (let [result (fetch-decoded-weather station)]
            (if (:success result)
              (let [weather-data (parse-weather-response (:success result) station)
                    report (format-weather-report weather-data username)]
                ;; Save station if report contains the station code
                (when (str/includes? report station)
                  (users/set-weather-station! username station))
                (msg/reply m report))
              (msg/reply m (cond
                             (= (:error result) :timeout)
                             (str "I got bored waiting for the weather report for " station)
                             (= (:error result) :invalid-station)
                             (str "That doesn't seem to be a valid location, " username ", sorry. See " codes-url)
                             :else
                             (str "Hmmmn. " username ", the NOAA weather server is giving me an error, sorry."))))))))))

;; ============================================================================
;; Module Definition
;; ============================================================================

(defmodule Weather
  :commands [:weather :weathar :waether :metar :rawmetar]

  (defn process-channel-message [m]
    (handle-weather m)))

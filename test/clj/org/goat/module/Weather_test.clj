(ns org.goat.module.Weather-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [org.goat.module.Weather :as sut]
            [org.goat.testutils.message :as msg-utils])
  (:import [org.goat.suntimes Time]))

;; ============================================================================
;; Parsing Tests
;; ============================================================================

(deftest test-extract-wind-with-gust
  (testing "Extract wind information with gust"
    (let [line "Wind: from the NW (310 degrees) at 15 MPH (13 KT) gusting to 25 MPH (22 KT)"
          result (#'sut/extract-wind line)]
      (is (= "NW" (:wind-direction result)))
      (is (= "15" (:wind-mph result)))
      (is (= "25" (:wind-gust result))))))

(deftest test-extract-wind-without-gust
  (testing "Extract wind information without gust"
    (let [line "Wind: from the SE (140 degrees) at 10 MPH (9 KT)"
          result (#'sut/extract-wind line)]
      (is (= "SE" (:wind-direction result)))
      (is (= "10" (:wind-mph result)))
      (is (nil? (:wind-gust result))))))

(deftest test-extract-coordinates
  (testing "Extract latitude and longitude"
    (let [line "Location: 40-38-23N 73-46-44W"
          result (#'sut/extract-coordinates line)]
      (is (number? (:latitude result)))
      (is (number? (:longitude result)))
      (is (> (:latitude result) 40))
      (is (< (:latitude result) 41))
      (is (< (:longitude result) -73))
      (is (> (:longitude result) -74)))))

(deftest test-extract-coordinates-southern-hemisphere
  (testing "Extract coordinates in southern hemisphere"
    (let [line "Location: 33-56S 151-10E"
          result (#'sut/extract-coordinates line)]
      (is (< (:latitude result) 0))
      (is (> (:longitude result) 0)))))

;; ============================================================================
;; Calculation Tests
;; ============================================================================

(deftest test-f-to-c-conversion
  (testing "Fahrenheit to Celsius conversion"
    (is (= 0.0 (#'sut/f-to-c 32.0)))
    (is (= 100.0 (#'sut/f-to-c 212.0)))
    (is (< (Math/abs (- 20.0 (#'sut/f-to-c 68.0))) 0.1))))

(deftest test-windchill-calculation
  (testing "Windchill calculation when applicable"
    (let [result (#'sut/windchill 40.0 15.0)]
      (is (< result 40.0))
      (is (> result 25.0)))))

(deftest test-windchill-not-applicable
  (testing "Windchill not applicable when temp > 50F or wind < 3mph"
    (is (= 60.0 (#'sut/windchill 60.0 15.0)))  ; Temp too high
    (is (= 40.0 (#'sut/windchill 40.0 2.0))))) ; Wind too low

(deftest test-parse-double-safe
  (testing "Safe double parsing"
    (is (= 123.45 (#'sut/parse-double-safe "123.45")))
    (is (= 0.0 (#'sut/parse-double-safe "invalid")))
    (is (= 0.0 (#'sut/parse-double-safe "")))))

(deftest test-calculate-sun-hours
  (testing "Calculate sun hours"
    (let [sunrise (Time. 6 0 0)
          sunset (Time. 18 0 0)]
      (is (= 12.0 (#'sut/calculate-sun-hours sunrise sunset))))))

(deftest test-calculate-sun-hours-crossing-midnight
  (testing "Calculate sun hours when crossing midnight"
    (let [sunrise (Time. 20 0 0)
          sunset (Time. 4 0 0)]
      (is (= 8.0 (#'sut/calculate-sun-hours sunrise sunset))))))

(deftest test-calculate-weather-bonus
  (testing "Weather bonus calculation"
    ;; Clear sky should have minimal bonus
    (is (< (#'sut/calculate-weather-bonus "clear" "") 2))
    ;; Tornado should have huge bonus
    (is (>= (#'sut/calculate-weather-bonus "" "tornado") 100))
    ;; Rain should add bonus
    (is (> (#'sut/calculate-weather-bonus "" "rain") 5))))

(deftest test-is-daylight
  (testing "Daylight detection"
    (let [sunrise (Time. 6 0 0)
          sunset (Time. 18 0 0)]
      ;; Noon should be daylight
      (is (true? (#'sut/is-daylight? 12 0 sunrise sunset)))
      ;; Midnight should not be daylight
      (is (false? (#'sut/is-daylight? 0 0 sunrise sunset)))
      ;; Dawn should not be daylight
      (is (false? (#'sut/is-daylight? 5 0 sunrise sunset)))
      ;; Evening should not be daylight
      (is (false? (#'sut/is-daylight? 20 0 sunrise sunset))))))

(deftest test-calculate-score
  (testing "Weather score calculation"
    (let [weather {:wind-mph "10"
                   :wind-gust "15"
                   :temperature-c "20"
                   :sky-conditions "clear"
                   :weather-type ""
                   :humidity "50%"}
          sunrise (Time. 6 0 0)
          sunset (Time. 18 0 0)
          score (#'sut/calculate-score weather sunrise sunset)]
      (is (number? score))
      (is (>= score 0)))))

;; ============================================================================
;; Astronomical Functions Tests
;; ============================================================================

(deftest test-get-sunrise-sunset
  (testing "Get sunrise and sunset times"
    ;; Test for New York (approx coordinates)
    (let [sunrise (#'sut/get-sunrise 2025 6 21 -73.78 40.64)
          sunset (#'sut/get-sunset 2025 6 21 -73.78 40.64)]
      (is (not (nil? sunrise)))
      (is (not (nil? sunset)))
      ;; Summer solstice sunrise should be early morning (UTC)
      (is (< (.getHour sunrise) 12))
      ;; Summer solstice sunset should be in evening (UTC, could be next day)
      (is (>= (.getHour sunset) 0)))))

(deftest test-format-sun-time
  (testing "Format sun time"
    (let [time-obj (Time. 14 30 0)
          formatted (#'sut/format-sun-time time-obj nil)]
      (is (str/includes? formatted ":"))
      (is (str/includes? formatted "pm")))))

(deftest test-moon-phase
  (testing "Moon phase functions return strings"
    (let [phase (#'sut/moon-phase)
          phase-short (#'sut/moon-phase-short)]
      (is (string? phase))
      (is (string? phase-short))
      (is (not (str/blank? phase)))
      (is (not (str/blank? phase-short))))))

;; ============================================================================
;; HTTP Functions Tests (basic structure tests - actual HTTP tested manually)
;; ============================================================================

(deftest test-fetch-metar-structure
  (testing "fetch-metar returns proper structure"
    (let [result (sut/fetch-metar "ZZZZ99")]  ; Truly invalid station
      (is (map? result))
      (is (or (contains? result :error) (contains? result :success))))))

(deftest test-fetch-decoded-weather-structure
  (testing "fetch-decoded-weather returns proper structure"
    (let [result (sut/fetch-decoded-weather "ZZZZ99")]  ; Truly invalid station
      (is (map? result))
      (is (or (contains? result :error) (contains? result :success))))))

;; ============================================================================
;; Command Integration Tests
;; ============================================================================

(deftest test-weather-command-no-saved-station
  (testing "Weather command with no saved station shows error"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "weather" ""
                  {:sender "testuser"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        (is (msg-utils/replied-with? "I don't know where you are"))
        (is (msg-utils/replied-with? "stations.txt"))))))

(deftest test-weather-command-recognizes-variants
  (testing "Weather command recognizes misspelling variants"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "weathar" ""
                  {:sender "testuser2"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))
        ;; Should process as weather command
        (is (msg-utils/replied-with? "where you are"))))))

(deftest test-metar-command-recognized
  (testing "METAR command is recognized"
    (msg-utils/with-clean-replies
      (let [msg (msg-utils/mock-command-message "metar" ""
                  {:sender "testuser3"})]
        (sut/process-channel-message nil msg)
        (is (= 1 (msg-utils/reply-count)))))))

;; ============================================================================
;; Formatting Tests
;; ============================================================================

(deftest test-format-weather-report-structure
  (testing "Weather report formatting structure"
    (let [weather-data {:station "KJFK"
                        :temperature-f "65"
                        :temperature-c "18"
                        :dew-point-c "12"
                        :wind-direction "NW"
                        :wind-mph "15"
                        :wind-gust "25"
                        :sky-conditions "partly cloudy"
                        :weather-type "light rain"
                        :precipitation "none"
                        :humidity "75%"
                        :latitude 40.64
                        :longitude -73.78
                        :report-year 2025
                        :report-month 6
                        :report-day 21
                        :report-hour 15
                        :report-minute 30
                        :minutes-ago 15
                        :processed-lines ["<b>Location:</b> John F Kennedy Intl Airport, NY, United States (KJFK) 40-38-23N 073-46-44W"
                                          "<b>Temperature:</b> 18 C"
                                          "<b>Dew Point:</b> 12 C"
                                          "<b>Wind:</b> from the NW (310 degrees) at 15 MPH (13 KT) gusting to 25 MPH (22 KT)"
                                          "<b>Sky conditions:</b> partly cloudy"
                                          "<b>Weather:</b> light rain"
                                          "<b>Relative Humidity:</b> 75%"]}
          report (#'sut/format-weather-report weather-data "testuser")]
      ;; Check for key fields from NOAA data
      (is (str/includes? report "Temperature:"))
      (is (str/includes? report "18 C"))
      (is (str/includes? report "Dew Point:"))
      (is (str/includes? report "12 C"))
      (is (str/includes? report "Wind:"))
      (is (str/includes? report "NW"))
      (is (str/includes? report "15 MPH"))
      (is (str/includes? report "gusting to 25 MPH"))
      (is (str/includes? report "Sky conditions:"))
      (is (str/includes? report "partly cloudy"))
      (is (str/includes? report "Weather:"))
      (is (str/includes? report "light rain"))
      (is (str/includes? report "Humidity:"))
      (is (str/includes? report "75%"))
      ;; Check for enhancements
      (is (str/includes? report "Sunrise:"))
      (is (str/includes? report "Sunset:"))
      (is (str/includes? report "Moon:"))
      (is (str/includes? report "Reported 15 minutes ago"))
      (is (str/includes? report "Score:")))))

(deftest test-format-raw-metar
  (testing "Raw METAR formatting"
    (let [metar "2025/12/11 15:30\nKJFK 111530Z 31015G25KT 10SM FEW250 18/12 A2990"
          formatted (#'sut/format-raw-metar metar "testuser")]
      (is (str/includes? formatted "KJFK"))
      (is (str/includes? formatted "15:30")))))

;; ============================================================================
;; Edge Cases and Error Handling
;; ============================================================================

(deftest test-parse-weather-with-missing-fields
  (testing "Parse weather data with missing fields gracefully"
    (let [minimal-html "Temperature: 20.0 F (6.7 C)"
          result (#'sut/parse-weather-response minimal-html "TEST")]
      (is (= "TEST" (:station result)))
      (is (= "20.0" (:temperature-f result)))
      (is (= "6.7" (:temperature-c result)))
      ;; Missing fields should have defaults
      (is (= "" (:wind-direction result)))
      (is (= "" (:wind-mph result))))))

(deftest test-empty-weather-data
  (testing "Handle empty weather data"
    (let [result (#'sut/parse-weather-response "" "TEST")]
      (is (= "TEST" (:station result)))
      (is (= "" (:temperature-c result))))))

(deftest test-temperature-conversion-to-celsius-only
  (testing "Temperature line converted to Celsius only"
    (let [html "Temperature: 44 F (7 C)"
          result (#'sut/parse-weather-response html "TEST")
          processed-lines (:processed-lines result)]
      (is (= 1 (count processed-lines)))
      (is (= "<b>Temperature:</b> 7 C" (first processed-lines)))
      (is (= "44" (:temperature-f result)))
      (is (= "7" (:temperature-c result))))))

(deftest test-dew-point-conversion-to-celsius-only
  (testing "Dew point line converted to Celsius only"
    (let [html "Dew Point: 44 F (7 C)"
          result (#'sut/parse-weather-response html "TEST")
          processed-lines (:processed-lines result)]
      (is (= 1 (count processed-lines)))
      (is (= "<b>Dew Point:</b> 7 C" (first processed-lines)))
      (is (= "7" (:dew-point-c result))))))

(deftest test-windchill-display
  (testing "Windchill is displayed when applicable"
    (let [weather-data {:station "TEST"
                        :temperature-f "44"
                        :temperature-c "7"
                        :wind-mph "5"
                        :latitude 55.5
                        :longitude -4.6
                        :report-year 2025
                        :report-month 12
                        :report-day 11
                        :report-hour 22
                        :report-minute 20
                        :minutes-ago 10
                        :processed-lines ["<b>Temperature:</b> 7 C"
                                          "<b>Wind:</b> from the SW at 5 MPH"]}
          report (#'sut/format-weather-report weather-data "testuser")]
      (is (str/includes? report "Windchill:"))
      (is (str/includes? report "C")))))

(deftest test-full-noaa-response-parsing
  (testing "Full NOAA response parsing with windchill"
    (let [noaa-response "Prestwick Airport, United Kingdom (EGPK) 55-30N 004-35W 0M
Dec 11, 2025 - 05:20 PM EST / 2025.12.11 2220 UTC
Wind: from the SW (220 degrees) at 5 MPH (4 KT) (direction variable):0
Visibility: greater than 7 mile(s):0
Sky conditions: mostly cloudy
Weather: light rain, drizzle
Temperature: 44 F (7 C)
Dew Point: 44 F (7 C)
Relative Humidity: 100%
Pressure (altimeter): 29.74 in. Hg (1007 hPa)
ob: EGPK 112220Z 22004KT 180V260 9999 -RADZ FEW006 BKN013 07/07 Q1007
cycle: 22"
          parsed (#'sut/parse-weather-response noaa-response "EGPK")
          report (#'sut/format-weather-report parsed "testuser")]
      ;; Check parsing
      (is (= "44" (:temperature-f parsed)))
      (is (= "7" (:temperature-c parsed)))
      (is (= "5" (:wind-mph parsed)))
      ;; Check windchill appears in report
      (is (str/includes? report "Windchill:")
          (str "Report should contain windchill. Report:\n" report)))))

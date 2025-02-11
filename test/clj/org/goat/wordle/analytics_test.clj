(ns org.goat.wordle.analytics-test
  (:require [org.goat.wordle.analytics :as sut]
            [clojure.test :as t :refer [deftest is testing]]))

;; This was a tricky fn so lets have a test for once
(deftest test-compare-guess-to-answer
  (is (= [:wrong :semiknown :wrong :semiknown :revealed]
         (sut/compare-guess-to-answer "FEELS" "PALES")))
  (is (= [:wrong :semiknown :wrong :revealed :revealed]
         (sut/compare-guess-to-answer "FLEES" "PALES")))
  (is (= [:wrong :revealed :revealed :revealed :wrong]
         (sut/compare-guess-to-answer "GLEES" "FLEET")))
  (is (= [:semiknown :semiknown :revealed :wrong :wrong]
         (sut/compare-guess-to-answer "EELSY" "AGLEE")))
  (is (= [:semiknown :semiknown :wrong :wrong :wrong]
         (sut/compare-guess-to-answer "EEESY" "AGLEE"))))

(deftest test-get-facts-basic
  (let [classfns '(:wrong :wrong :wrong :wrong :revealed)
        facts (sut/get-facts classfns "ABCDE")]
    (is (= {\A {:upper 0},
            \B {:upper 0},
            \C {:upper 0},
            \D {:upper 0},
            \E {:lower 1}}
           (:bounds facts))
        "A-D should have an upper bound of 0, E should have an unknown upper bound and a lower bound of 1")
    (is (= {4 \E}
           (:known facts))
        "E is known at position 4 and nothing else")
    (is (= {0 #{\A}, 1 #{\B}, 2 #{\C}, 3 #{\D}}
           (:known-nots facts))
        "A is known to not be at pos 0, B is known to not be at pos 1 etc.")))

(deftest test-get-facts-semiknown
  (let [classfns '(:wrong :wrong :semiknown :semiknown :revealed)
        facts (sut/get-facts classfns "EBCDE")]

    (is (= {\E {:upper 1 :lower 1},
            \B {:upper 0},
            \C {:lower 1},
            \D {:lower 1}}
           (:bounds facts))
        "E should have an upper and lower bound of 1. B should have upper bound 0. C and D should have lower bound of 1.")
    (is (= {4 \E}
           (:known facts))
        "E is known at position 4 and nothing else")
    (is (= {0 #{\E}, 1 #{\B}, 2 #{\C}, 3 #{\D} }
           (:known-nots facts))
        "E is known to not be at pos 0, B is known to not be at pos 1, and C&D as semiknown are known to not be at 2 and 3")))

(deftest test-get-more-facts
  (let [classfns1 '(:wrong :wrong :semiknown :semiknown :revealed)
        facts1 (sut/get-facts classfns1 "EBCDE")
        classfns2 '(:wrong :revealed :semiknown :wrong :revealed)
        facts2 (sut/get-facts classfns2 "XCDXE" facts1 0)]
    (is (= {4 \E, 1 \C} ; Position 1 maps to C, position 4 to E
           (:known facts2))
        "C added at pos 1, E remains at pos 4")
    ;; fails because both \C and \D are incorrectly bounded as {:lower 1 :upper 2147483647} suggesting an overflow!
    ;; other letters are correct
    (is (= {\E { :upper 1 :lower 1 },
            \B { :upper 0 },
            \C { :lower 1 },
            \D { :lower 1 },
            \X { :upper 0 }}
           (:bounds facts2))
        "C and D are still lower 1, X is resolved to upper 0, E remains are upper 1 lower 1")
    (is (= {0 #{\E \X}, 1 #{\B}, 2 #{\C \D}, 3 #{\D \X}}
           (:known-nots facts2))
        ":known-nots should be as listed")
    ))

(deftest test-word-matches-known?
  (is (and
       (sut/word-matches-known? {0 \A 4 \A} "AXXXA")
       (sut/word-matches-known? {0 \A 2 \A 3 \X} "AXAXA")
       (sut/word-matches-known? {1 \X 4 \A} "AXXXA")
       (sut/word-matches-known? {3 \Q 4 \M 0 \X 2 \Z 1 \Y} "XYZQM")
       (sut/word-matches-known? {} "AXXXA"))
      "These should all match correctly.")
  (is (not
       (or
        (sut/word-matches-known? {0 \A 3 \A} "AXXXA")
        (sut/word-matches-known? {0 \A 2 \A 4 \X} "AXAXA")
        (sut/word-matches-known? {0 \X 4 \A} "AXXXA")
        (sut/word-matches-known? {3 \Q 4 \Q 0 \X 2 \Z 1 \Y} "XYZQM")
        (sut/word-matches-known? {4 \X} "AXXXA")))
      "None of these should match"))

(deftest test-word-matches-known-nots?
  (is (and
       (sut/word-matches-known-nots? {0 #{\Z \B} 4 #{\B \C}} "AXXXA")
       (sut/word-matches-known-nots? {1 #{\A \C \D \E} 4 #{\A \B \C \D}} "ABCDE")
       (sut/word-matches-known-nots? {0 #{\X} 4 #{\B \C} 1 #{\Z} 2 #{\Q} 3 #{\M} } "AXXXA")
       (sut/word-matches-known-nots? {4 #{\X}} "AXXXA"))
      "These should all match correctly.")
  (is (not
       (or
        (sut/word-matches-known-nots? {4 #{\A}} "AXXXA")
        (sut/word-matches-known-nots? {0 #{\A} 1 #{\X} 2 #{\X} 3 #{\X} 4 #{\A} } "AXXXA")
        (sut/word-matches-known-nots? {0 #{\A \B} 1 #{\B \C \D} 2 #{\B \C} 3 #{\M \D} 4 #{\E \F \B} } "ABCDE")))
      "None of these should match"))

(deftest test-word-matches-bounds?
  (let [classfns (sut/classify-letters "AABBX" "AXAZB") ;;rvd smi smi wrg smi
        facts (sut/get-facts classfns "AABBX")
        bounds (:bounds facts)]
    (is (and
         (sut/word-matches-bounds? bounds "AXAZB")
         (sut/word-matches-bounds? bounds "AABQX")
         (sut/word-matches-bounds? bounds "AXABA")
         (sut/word-matches-bounds? bounds "ABNAX")
         (sut/word-matches-bounds? {} "POOPS")))
    (is (not (or
         (sut/word-matches-bounds? bounds "AABBX")
         (sut/word-matches-bounds? bounds "AXBBX")
         (sut/word-matches-bounds? bounds "AXBBA")
         (sut/word-matches-bounds? bounds "AXQQA")
         (sut/word-matches-bounds? bounds "AZBBA"))))))

(deftest test-optimal-guesses
  (testing "optimal-guesses with a custom dict-set"
    (with-redefs [sut/dict-set (set '("AA" "AC" "CC" "CD" "CX" "XC" "QQ" "AB"))]
      (with-redefs [sut/indexes (sut/build-indexes sut/dict-set)]
        (let [optimals (sut/optimal-guesses sut/dict-set) ;; AC is best, QQ is worst
              best (ffirst (sort-by val < optimals))
              worst (ffirst (sort-by val > optimals))]
          (is (= "AC" best) "AC should be the BEST choice")
          (is (= "QQ" worst) "QQ should be the WORST choice"))))))
  
(deftest test-ranking-guesses
  (let [answer "MOPER"
        guess "HOPER"
        facts (sut/get-facts (sut/classify-letters guess answer) guess)
        allowed-words (sut/allowed-words-for-facts facts)
        valid-guesses (sut/valid-guess-words facts answer)
        ranked-guesses (sut/rank-guesses allowed-words valid-guesses)]
    (is (= 5 (count allowed-words)) "There should be 5 possible answer words.")
    (is (= 4111 (count valid-guesses)) "There should be 4111 information-revealing guesses possible")
    (is (= 4111 (count ranked-guesses)) "So there should also be 4111 ranked guesses")
    (is (= 52 (count (filter #(> (second %) 1.33) ranked-guesses))) "There should be 52 best possible guesses")
    (is (contains? (set (map first (filter #(> (second %) 1.33) ranked-guesses))) "MELTS") "MELTS should be a best possible guess")
    (is (not (contains? (set (map first (filter #(> (second %) 1.33) ranked-guesses))) answer)) "The correct answer is NOT a best possible guess")))

(deftest bmark
  (let [answer "DOPER"
       guess "SLANT"
       guess2 "GRIPE"
       facts (sut/get-facts (sut/classify-letters guess answer) guess)
       facts2 (sut/add-to-facts facts guess2 answer)
       ]

	(println "allowed-words-for-facts-count: " (count (sut/allowed-words-for-facts facts)))
   (time (count (sut/allowed-words-for-facts facts)))
   (println "allowed-words-for-facts2-count: " (count (sut/allowed-words-for-facts facts2)))
   (time (count (sut/allowed-words-for-facts facts2)))
	;;72 seconds for the LLM iterated version.. 7 seconds for hand crafted one
	;;humans win?
   (println "rate-guess timing:")
   (time (sut/rate-guess guess2 answer facts))
   )
  )

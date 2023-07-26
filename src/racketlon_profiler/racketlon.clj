(ns racketlon
  (:require [racketlon-constants :as constants]))

; (java.lang.System/getProperty "java.class.path")

;; COMBINATORICS
;; by Mark Engelberg (mark.engelberg@gmail.com)
;; Last updated - October 24, 2011

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
         iter-comb
         (fn iter-comb [c j]
           (if (> j n) nil
               (let [c (assoc c j (dec (c j)))]
                 (if (< (c j) j) [c (inc j)]
                     (loop [c c, j j]
                       (if (= j 1) [c j]
                           (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
         step
         (fn step [c j]
           (cons (rseq (subvec c 1 (inc n)))
                 (lazy-seq (let [next-step (iter-comb c j)]
                             (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
        (let [cnt (count items)]
          (cond (> n cnt) nil
                (= n cnt) (list (seq items))
                :else
                (map #(map v-items %) (index-combinations n cnt)))))))

(combinations [1 2 3] 2)

(defn win-rally 
  [prob]
  (< (rand) prob))

(defn simulate-TT-BA-SQ-set
  [prob-win-point set-name]
  (loop [p1-ppts 0
         p2-ppts 0]
    ; a set is completed when a player reaches at least 21 and the difference is at least 2 
    (if (and (or (>= p1-ppts 21) (>= p2-ppts 21))
             (>= (Math/abs (- p1-ppts p2-ppts)) 2))
      ; the set is over! Print and return the score.
      (do
        (println set-name "score" p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      ; Play another point and update the score
      (let [win-point? (win-rally prob-win-point)]
        (recur (if win-point? (inc p1-ppts) p1-ppts)
               (if (not win-point?) (inc p2-ppts) p2-ppts))))))

(defn simulate-TE-set
  [prob-win-point diff-before]
  (loop [p1-ppts 0
         p2-ppts 0]
    ; if the absolute diff is less than or equal to 1, a full set is played  
    (if (and (<= (Math/abs diff-before) 1)
             (or (>= p1-ppts 21) (>= p2-ppts 21))
             (>= (Math/abs (- p1-ppts p2-ppts)) 2))
      (do
        (println "TE score: " p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      ; if player 1 is in the lead, 22 - the difference is their win condition 
      (if (and (> diff-before 1)
               (or (>= p1-ppts (- 22 diff-before)) (>= p2-ppts 21)))
        (do
          (println "TE score" p1-ppts "-" p2-ppts)
          [p1-ppts p2-ppts])
        ; if player 2 is in the lead, 22 - the absolute value of the difference is their win condition
        (if (and (< diff-before -1)
                 (or (>= p1-ppts 21) (>= p2-ppts (- 22 (Math/abs diff-before)))))
          (do
            (println "TE score" p1-ppts "-" p2-ppts)
            [p1-ppts p2-ppts])
          (let [win-point? (win-rally prob-win-point)]
            (recur (if win-point? (inc p1-ppts) p1-ppts)
                   (if (not win-point?) (inc p2-ppts) p2-ppts))))))))

(defn simulate-match
  [& args]
  (let [probabilities (map read-string args) ; Convert probability strings to integers
        set-names ["TT" "BA" "SQ"]
        TE-prob (read-string (nth args 3))
          ; simulate TT, BA, and SQ sets and sum up the points
        [p1-score p2-score] (reduce
                             (fn [[p1 p2] [prob set-name]]
                               (let [[p1-ppts p2-ppts] (simulate-TT-BA-SQ-set prob set-name)]
                                 [(+ p1 p1-ppts) (+ p2 p2-ppts)]))
                             [0 0]
                             (map vector probabilities set-names))]

    ; simulate a TE set if needed
    (if (<= (Math/abs (- p1-score p2-score)) 21)
      (simulate-TE-set TE-prob (- p1-score p2-score))
      (println "No TE needed."))

    ; simulate a gummiarm point if needed
    (if (= p1-score p2-score)
      (let [gummiwinner (if (win-rally TE-prob) 1 2)]
        (let [new-p1-score (if (= gummiwinner 1) (inc p1-score) p1-score)
              new-p2-score (if (= gummiwinner 2) (inc p2-score) p2-score)]
          (println "- Gummiarm tiebreak played.")
          (println "Match score:" new-p1-score "-" new-p2-score)
          (- new-p1-score new-p2-score)))
      ; no gummiarm point, just sum up the set scores
      (do
        (println "Match score:" p1-score "-" p2-score)
        (- p1-score p2-score)))))

(defn find-best-worst-opponent-profile
  [arg-TT arg-BA arg-SQ arg-TE]
   (let [ratings (map #(Integer/parseInt %) [arg-TT arg-BA arg-SQ arg-TE])
         total-strength (reduce + ratings)] ; sum up the skill points you have

     (println "You are playing like this:" ratings " with a total strength of " total-strength)

     ; create all possible opponent profiles
     (let [profs (combinations [1 2 3 4 5] 4)]
       (println "Total number of possible player profiles:" profs)

     ; simulate matches against all of them
       (calculate-player1-win-percentages ratings (nth racketlon-constants/all_combinations 624))

     ; identify the largest and smallest median differences

     ; print the results
       (println "Here are the results!")

     )))

(find-best-worst-opponent-profile "2" "4" "5" "1")

; input your point win probabilities per sport
(simulate-match "0.35" "0.7" "0.8" "0.25")
(simulate-match "0.8" "0.8" "0.8" "0.7")
(simulate-match "0.5" "0.5" "0.5" "0.5")

(defn calculate-median-score
  ; simulate 1,000 matches and return the median score
  [TT-prob BA-prob SQ-prob TE-prob]
  ; call simulate-match nbr-matches times and sum up the differences
  (let [nbr-matches 1000
        differences (vec (repeatedly nbr-matches
                                     #(simulate-match TT-prob BA-prob SQ-prob TE-prob)))]
    ; sort the differences and return the median
    (let [sorted-differences (sort differences)
            middle (quot (count sorted-differences) 2)]
        (if (even? (count sorted-differences))
          (/ (+ (nth sorted-differences middle)
                (nth sorted-differences (dec middle)))
             2)
          (nth sorted-differences middle)))))

(calculate-median-score "0.35" "0.7" "0.8" "0.25")

(defn convert-diff-to-percentage 
  ; convert the differences in rating to percentages
  [differences] 
  (let [transform-first (cond
                          (= (nth differences 0) -4) (- 1 racketlon-constants/TT-substantial)
                          (= (nth differences 0) -3) (- 1 racketlon-constants/TT-major)
                          (= (nth differences 0) -2) (- 1 racketlon-constants/TT-minor)
                          (= (nth differences 0) -1) (- 1 racketlon-constants/TT-marginal)
                          (= (nth differences 0) 0) 0.5
                          (= (nth differences 0) 1) racketlon-constants/TT-marginal
                          (= (nth differences 0) 2) racketlon-constants/TT-minor
                          (= (nth differences 0) 3) racketlon-constants/TT-major
                          (= (nth differences 0) 4) racketlon-constants/TT-substantial
                          :else :unknown)

        transform-second (cond
                           (= (nth differences 1) -4) (- 1 racketlon-constants/BA-substantial)
                           (= (nth differences 1) -3) (- 1 racketlon-constants/BA-major)
                           (= (nth differences 1) -2) (- 1 racketlon-constants/BA-minor)
                           (= (nth differences 1) -1) (- 1 racketlon-constants/BA-marginal)
                           (= (nth differences 1) 0) 0.5
                           (= (nth differences 1) 1) racketlon-constants/BA-marginal
                           (= (nth differences 1) 2) racketlon-constants/BA-minor
                           (= (nth differences 1) 3) racketlon-constants/BA-major
                           (= (nth differences 1) 4) racketlon-constants/BA-substantial
                           :else :unknown)

        transform-third (cond
                          (= (nth differences 2) -4) (- 1 racketlon-constants/SQ-substantial)
                          (= (nth differences 2) -3) (- 1 racketlon-constants/SQ-major)
                          (= (nth differences 2) -2) (- 1 racketlon-constants/SQ-minor)
                          (= (nth differences 2) -1) (- 1 racketlon-constants/SQ-marginal)
                          (= (nth differences 2) 0) 0.5
                          (= (nth differences 2) 1) racketlon-constants/SQ-marginal
                          (= (nth differences 2) 2) racketlon-constants/SQ-minor
                          (= (nth differences 2) 3) racketlon-constants/SQ-major
                          (= (nth differences 2) 4) racketlon-constants/SQ-substantial
                          :else :unknown)

        transform-fourth (cond
                           (= (nth differences 3) -4) (- 1 racketlon-constants/TE-substantial)
                           (= (nth differences 3) -3) (- 1 racketlon-constants/TE-major)
                           (= (nth differences 3) -2) (- 1 racketlon-constants/TE-minor)
                           (= (nth differences 3) -1) (- 1 racketlon-constants/TE-marginal)
                           (= (nth differences 3) 0) 0.5
                           (= (nth differences 3) 1) racketlon-constants/TE-marginal
                           (= (nth differences 3) 2) racketlon-constants/TE-minor
                           (= (nth differences 3) 3) racketlon-constants/TE-major
                           (= (nth differences 3) 4) racketlon-constants/TE-substantial
                           :else :unknown)]

    [transform-first transform-second transform-third transform-fourth]))

(defn calculate-player1-win-percentages
  ; calculate win percentages for player 1 when playing against player 2
  [player1-profile player2-profile]
  (let [player-diffs (map - player1-profile player2-profile)]
    (convert-diff-to-percentage player-diffs)))

(def player-markus [2 4 5 1])
(def player-niklas [5 3 3 4])

(calculate-player1-win-percentages player-markus player-niklas)

(def markus-niklas (calculate-player1-win-percentages player-markus player-niklas))
(def markus-TT (str (first markus-niklas)))
(def markus-BA (str (second markus-niklas)))
(def markus-SQ (str (nth markus-niklas 2)))
(def markus-TE (str (nth markus-niklas 3)))

(calculate-median-score markus-TT markus-BA markus-SQ markus-TE)

(println (nth racketlon-constants/all_combinations 432))
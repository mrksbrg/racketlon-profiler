(ns racketlon
  (:require [racketlon-constants :as constants]))

; (java.lang.System/getProperty "java.class.path")

(defn win-rally
  ; truthy if random value is smaller than prob
  [prob]
  (< (rand) prob))

(defn simulate-TT-BA-SQ-set
  ; simulate a set of set-name for which the player wins each rally with the probability prob-win-point
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
    ; simulate a set of tennis for which the player wins each rally with the probability prob-win-point
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
  [probabilities]
  ; simulate a racketlon match for which the player has the probabilities to win rallies
  (let [set-names ["TT" "BA" "SQ"]
        TE-prob (nth probabilities 3)
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

(defn sum-to-n?
  ; predicate function to check if the sum of elements is equal to n
  [n combination]
  (= n (apply + combination)))

(defn profiles-summing-to-n
  ; among the 625 possible player profiles, filter out the ones that sum up to n
  [n]
  (filter #(sum-to-n? n %) racketlon-constants/all_combinations))

(defn find-best-worst-opponent-profile
  [ratings]
  (let [total-strength (reduce + ratings)] ; sum up the skill points you have
    (println "You are playing like this:" ratings "with a total strength of" total-strength)
         ; find all possible opponent profiles 
    (let [possible-opponents (profiles-summing-to-n total-strength)]
      (println "Number of player profiles matching your strength:" (count possible-opponents))
          ; calculate win rally percentages against them  
      (let [percentages (map #(calculate-player1-win-percentages ratings %) possible-opponents)
            profiles (zipmap percentages ; <-- Switch the order of key-value pairs
                             possible-opponents)]
            ; call calculate-median-score for each entry in possible-opponents and store the results
        (let [median-scores (doall (map #(hash-map :rally-percentages % :median-score (calculate-median-score %)) percentages))]
          (println "There are" (count median-scores) "opponent profiles that match your overall racketlon strength.")
               ; identify the 5 best and 5 worst median scores
          (let [sorted-median-scores (sort-by :median-score median-scores)]
            (println "Median scores against the 5 most favorable opponent profiles:")
            (doseq [entry (take-last 5 sorted-median-scores)]
              (let [rally-percentages (:rally-percentages entry)
                    opponent-profile (get profiles rally-percentages)]
                (println "Score:" (:median-score entry) "Profile:" opponent-profile)))

            (println "Median scores against the 5 most unfavorable opponent profiles:")
            (doseq [entry (take 5 sorted-median-scores)]
              (let [rally-percentages (:rally-percentages entry)
                    opponent-profile (get profiles rally-percentages)]
                (println "Score:" (:median-score entry) "Profile:" opponent-profile)))))))))

(defn calculate-median-score
  ; simulate 1000 matches and return the median score
  [probabilities]
  (println "calculate-median-score:" probabilities)
  ; call simulate-match 1000 times and sum up the differences
  (let [nbr-matches 1000
        differences (vec (repeatedly nbr-matches
                                     #(simulate-match probabilities)))]
    ; sort the differences and return the median
    (let [sorted-differences (sort differences)
          middle (quot (count sorted-differences) 2)]
      (if (even? (count sorted-differences))
        (/ (+ (nth sorted-differences middle)
              (nth sorted-differences (dec middle)))
           2)
        (nth sorted-differences middle)))))

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

(defn vector-of-four [integers]
  (vector (first integers) (second integers) (nth integers 2) (nth integers 3)))

(defn -main
  ; entry point for the program
  [& args]
  (if (not= (count args) 4)
    (println "Provide four integers as arguments")
    (let [integers (map #(Integer/parseInt %) args)
          vector-result (vector-of-four integers)]
      (find-best-worst-opponent-profile vector-result))))

(-main "2" "3" "4" "1")
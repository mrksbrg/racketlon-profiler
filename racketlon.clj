(ns racketlon
  (:require [racketlon.constants :as constants]))

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
  (let [probabilities (map read-string args)
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

; input your point win probabilities per sport
(simulate-match "0.35" "0.7" "0.8" "0.25")
(simulate-match "0.8" "0.8" "0.8" "0.7")
(simulate-match "0.5" "0.5" "0.5" "0.5")

(defn test-profile
  ; test a single racketlon profile
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

(test-profile "0.35" "0.7" "0.8" "0.25")


(ns racketlon)

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
        ; simulate TT, BA, and SQ sets and sum up the points
        scores (reduce
                (fn [acc [prob set-name]]
                  (let [[p1-ppts p2-ppts] (simulate-TT-BA-SQ-set prob set-name)]
                    [(+ (first acc) p1-ppts) (+ (second acc) p2-ppts)]))
                [0 0]
                (map vector probabilities set-names))]
    ; simulate a TE set if needed
    (if (<= (Math/abs (- (first scores) (second scores))) 21)
      (simulate-TE-set (read-string (nth args 3)) (- (first scores) (second scores)))
      (println "No TE needed."))
    ; simulate a gummiarm point if needed
    ;(if (= (first scores) (second scores))
    ;  (if (win-rally (read-string (nth args 3)))
    ;  (println "P1 won by gummi!"))
    ;  (println "P2 won by gummi"))
    (if (> (first scores) (second scores))
      (println "You won!")
      (println "You lost."))
    (println "Match score:" (first scores) "-" (second scores))
    (- (first scores) (second scores))))

; input your point win probabilities per sport
(simulate-match "0.35" "0.7" "0.8" "0.25")
(simulate-match "0.8" "0.8" "0.8" "0.7")
(simulate-match "0.5" "0.5" "0.5" "0.5")

(defn simulate-match-distribution
  [& args]
  (let [num-matches 1000000
        scores (vec (repeatedly num-matches (partial apply simulate-match args)))
        frequency-map (frequencies scores)
        sorted-frequency (sort-by (comp - second) frequency-map)]
    sorted-frequency))

(simulate-match-distribution "0.35" "0.7" "0.8" "0.25")
(simulate-match-distribution "0.5" "0.5" "0.5" "0.5")


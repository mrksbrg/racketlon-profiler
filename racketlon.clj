(ns racketlon)

(defn win-rally [prob]
  (< (rand) prob))

(defn simulate-simple-set [prob-win-point set-name]
  (loop [p1-ppts 0
         p2-ppts 0]
    (if (and (>= p1-ppts 21) (>= (- p1-ppts p2-ppts) 2))
      (do
        (println set-name "score" p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      (if (and (>= p2-ppts 21) (>= (- p2-ppts p1-ppts) 2))
        (do
          (println set-name "score" p1-ppts "-" p2-ppts)
          [p1-ppts p2-ppts])
        (if (and (= p1-ppts 1) (= p2-ppts 1))
          (do
            (println "P1=P2")
            [p1-ppts p2-ppts])
          (let [win-point? (win-rally prob-win-point)]
            (recur (if win-point? (inc p1-ppts) p1-ppts)
                   (if (not win-point?) (inc p2-ppts) p2-ppts))))))))

(simulate-TT-BA-SQ-set 0.5 "TT")

(defn simulate-TT-BA-SQ-set [prob-win-point set-name]
  (loop [p1-ppts 0
         p2-ppts 0]
    ; a set is completed when a player reaches at least 21 and the difference is at least 2 
    (if (and (or (>= p1-ppts 21) (>= p2-ppts 21))
             (>= (Math/abs (- p1-ppts p2-ppts)) 2))
      (do
        ; the set is over! Print and return the score.
        (println set-name "score" p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      ; Play another point and update the score
      (let [win-point? (win-rally prob-win-point)]
        (recur (if win-point? (inc p1-ppts) p1-ppts)
               (if (not win-point?) (inc p2-ppts) p2-ppts))))))

(defn simulate-TE-set [prob-win-point diff-before]
  (loop [p1-ppts 0
         p2-ppts 0]
    (cond
      ; if the absolute diff is less than or equal to 1, a full set is played
      (and (<= (Math/abs diff-before) 1) (and (>= p1-ppts 21) (>= (Math/abs (- p1-ppts p2-ppts)) 2)))
      (do
        (println "TE score full" p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      ; if player 1 is in the lead, 22 - the difference is the win condition
      (and (pos? diff-before) (or (>= p1-ppts 21) (>= p2-ppts 21) (>= p1-ppts (- 22 diff-before))))
      (do
        (println "TE score" p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      ; if player 2 is in the lead, 22 - the absolute value of the difference is the win condition
      (and (neg? diff-before) (or (>= p2-ppts 21) (>= p1-ppts 21) (>= p2-ppts (- 22 (Math/abs diff-before)))))
      (do
        (println "TE score" p1-ppts "-" p2-ppts)
        [p1-ppts p2-ppts])
      :else
      (let [win-point? (win-rally prob-win-point)]
        (recur (if win-point? (inc p1-ppts) p1-ppts)
               (if (not win-point?) (inc p2-ppts) p2-ppts))))))

(simulate-TE-set 0.5 1)

  (defn simulate-set [prob-win-point set-name]
    (let [[player1-points player2-points] (simulate-TT-BA-SQ-set prob-win-point)]
      (println set-name "score" player1-points "-" player2-points)
      [player1-points player2-points]))

  (defn simulate-match [& args]
    (let [probabilities (map read-string args)
          set-names ["TT" "BA" "SQ"]
          scores (reduce
                  (fn [acc [prob set-name]]
                    (let [[p1-ppts p2-ppts] (simulate-TT-BA-SQ-set prob set-name)]
                      [(+ (first acc) p1-ppts) (+ (second acc) p2-ppts)]))
                  [0 0]
                  (map vector probabilities set-names))]
      (if (> (first scores) (second scores))
        (println "You won!")
        (println "You lost."))
      (println "Match score:" (first scores) "-" (second scores))
      (- (first scores) (second scores))))

(simulate-match "0.35" "0.7" "0.8" "0.25")
; input your point win probabilities per sport

  (defn simulate-match-distribution [& args]
    (let [num-matches 1000
          scores (vec (repeatedly num-matches (partial apply simulate-match args)))]
      (frequencies scores)))

  (println "Score Distribution:")
  (doseq [[score count] (sort-by first score-distribution)]
    (println "Score:" score "Count:" count))

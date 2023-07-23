; racketlon-constants.clj

(ns racketlon-constants)

; MODEL RACKETLON PLAYERS' PROFILES AS VECTORS OF 4 INTEGERS BETWEEN 1 AND 5
; THE INTEGERS REPRESENT THE PLAYERS' RATING IN THE FOUR SPORTS, I.E.,
; TABLE TENNIS, BADMINTON, SQUASH, AND TENNIS. HIGHER NUMBER MEANS 
; A HIGHER RATING.
;
; ONE WAY TO SELECT A RATING IS TO LOOK AT THE OVERALL WIN PERCENTAGE OVER 
; VERY MANY MATCHES. AS A RULE OF THUMB, THE FOLLOWING FRACTIONS OF WON RALLIES
; CAN BE USED:
; 1: <= 30%, 2: 30-50%, 3: 50-60%, 4: 60-75%, 5: >=75%

(defn generate-combinations
  ; generate the 625 different player profiles that exist in racketlon
  []
  (for [i (range 1 6)
        j (range 1 6)
        k (range 1 6)
        l (range 1 6)]
    [i j k l]))

(def all_combinations (generate-combinations))

; LOSERS POINTS / WINNERS PERCENTAGES ACCORDING TO "RACKET COUSINS"
; MARGINAL, MINOR, MAJOR, SUBSTANTIAL
; TT: 13.1 / 0.6158, 7.6 / 0.7343, 4.4 / 0.8268, 4.0 / 0.84
; BA: 15.3 / 0.5785, 11.5 / 0.6462, 8.6 / 0.7095, 6.9 / 0.7527
; SQ: 16.3 / 0.5630, 10.2 / 0.6731, 7.9 / 0.7266, 6.2 / 0.7721
; TE: 16.8 / 0.5556, 13.0 / 0.6176, 11.1 / 0.6542, 11.0 / 0.6563

(def TT-marginal (/ 21 (+ 21 13.1)))
(def BA-marginal (/ 21 (+ 21 15.3)))
(def SQ-marginal (/ 21 (+ 21 16.3)))
(def TE-marginal (/ 21 (+ 21 16.8)))
(def TT-minor (/ 21 (+ 21 7.6)))
(def BA-minor (/ 21 (+ 21 11.5)))
(def SQ-minor (/ 21 (+ 21 10.2)))
(def TE-minor (/ 21 (+ 21 13.0)))
(def TT-major (/ 21 (+ 21 4.4)))
(def BA-major (/ 21 (+ 21 8.6)))
(def SQ-major (/ 21 (+ 21 7.9)))
(def TE-major (/ 21 (+ 21 11.1)))
(def TT-substantial (/ 21 (+ 21 4.0)))
(def BA-substantial (/ 21 (+ 21 6.9)))
(def SQ-substantial (/ 21 (+ 21 6.2)))
(def TE-substantial (/ 21 (+ 21 11.0)))
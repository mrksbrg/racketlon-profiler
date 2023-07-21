; racketlon-constants.clj

(ns racketlon-constants)

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
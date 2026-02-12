;;; spatial-window-geometry-voronoi-test.el --- Tests for Voronoi variant -*- lexical-binding: t; -*-

;;; Commentary:
;; Invariant tests for spatial-window-geometry-voronoi.el

;;; Code:

(require 'ert)
(require 'spatial-window-geometry-voronoi)
(require 'spatial-window-geometry-variant-test-common)

(ert-deftest spatial-window-voronoi-invariants ()
  "Run invariant checks across canonical layouts."
  (spatial-window--variant-assert-basic '((win 0.0 1.0 0.0 1.0)) "single-window")
  (spatial-window--variant-assert-basic
   '((win-left 0.0 0.5 0.0 1.0)
     (win-right 0.5 1.0 0.0 1.0))
   "2-columns")
  (spatial-window--variant-assert-basic
   '((win-top-left 0.0 0.5 0.0 0.5)
     (win-bottom-left 0.0 0.5 0.5 1.0)
     (win-right 0.5 1.0 0.0 1.0))
   "2-left-1-right")
  (spatial-window--variant-assert-basic
   '((win-left 0.0 0.2 0.0 1.0)
     (win-mid-top 0.2 0.7 0.0 0.5)
     (win-mid-bot 0.2 0.7 0.5 1.0)
     (win-right 0.7 1.0 0.0 1.0))
   "3-columns")
  (spatial-window--variant-assert-basic
   '((win-main 0.0 0.955 0.0 1.0)
     (win-sidebar-top 0.955 1.0 0.0 0.92)
     (win-sidebar-bot 0.955 1.0 0.92 1.0))
   "extreme-split")
  (spatial-window--variant-assert-basic
   '((win-magit 0.0 0.511 0.0 0.483)
     (win-claude 0.511 1.0 0.0 1.0)
     (win-sw1 0.0 0.066 0.483 0.725)
     (win-sw2 0.066 0.129 0.483 1.0)
     (win-sw3 0.129 0.255 0.483 1.0)
     (win-sw4 0.255 0.511 0.483 1.0)
     (win-backtrace 0.0 0.066 0.725 1.0))
   "complex-spanning")
  (spatial-window--variant-assert-basic
   '((win-main 0.0 0.63 0.0 0.93)
     (win-diff 0.0 0.63 0.93 0.985)
     (win-claude 0.63 1.0 0.0 0.985))
   "ide-layout-thin-panel")
  (spatial-window--variant-assert-basic
   '((win-top-left 0.001 0.042 0.002 0.769)
     (win-bot-left 0.001 0.042 0.769 0.985)
     (win-right 0.042 0.999 0.002 0.985))
   "extreme-narrow-left")
  (spatial-window--variant-assert-basic
   '((win-top-left 0.001 0.598 0.002 0.5)
     (win-top-right 0.598 0.999 0.002 0.5)
     (win-bot-left 0.001 0.327 0.5 0.985)
     (win-bot-right 0.327 0.999 0.5 0.985))
   "misaligned-vertical")
  (spatial-window--variant-assert-basic
   '((win-posframe-top 0.3192982456140351 0.9988304093567252 0.48653846153846153 0.7423076923076923)
     (win-posframe-bot 0.3192982456140351 0.9988304093567252 0.7423076923076923 0.9846153846153847)
     (win-code-narrow 0.0011695906432748538 0.11052631578947368 0.0019230769230769232 0.48653846153846153)
     (win-code-wide 0.11052631578947368 0.9988304093567252 0.0019230769230769232 0.48653846153846153)
     (win-magit 0.0011695906432748538 0.3192982456140351 0.48653846153846153 0.9846153846153847))
   "real-dev-session"))

(provide 'spatial-window-geometry-voronoi-test)

;;; spatial-window-geometry-voronoi-test.el ends here

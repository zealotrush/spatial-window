;;; spatial-window-geometry-variant-test-common.el --- Shared checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared checks for geometry variants. These tests assert invariants
;; (keys are valid, unique, and each window is reachable) without
;; enforcing the exact key pattern.

;;; Code:

(require 'ert)
(require 'spatial-window) ; for spatial-window--get-layout

(defun spatial-window--variant-assert-basic (window-bounds &optional msg)
  "Assert basic invariants for WINDOW-BOUNDS in current geometry module."
  (let* ((result (spatial-window--assign-keys nil window-bounds))
         (layout (apply #'append (spatial-window--get-layout)))
         (layout-set (make-hash-table :test 'equal))
         (seen (make-hash-table :test 'equal))
         (windows (mapcar #'car window-bounds))
         (all-keys nil))
    (dolist (k layout) (puthash k t layout-set))
    (should (and result (listp result)))
    ;; Each window appears and has at least one key
    (dolist (w windows)
      (let ((keys (cdr (assq w result))))
        (should (and keys (consp keys)))))
    ;; Keys are valid and unique
    (dolist (pair result)
      (dolist (k (cdr pair))
        (when msg (should msg))
        (should (gethash k layout-set))
        (push k all-keys)
        (should (not (gethash k seen)))
        (puthash k t seen)))
    ;; No key outside layout
    (dolist (k all-keys)
      (should (gethash k layout-set)))))

(provide 'spatial-window-geometry-variant-test-common)

;;; spatial-window-geometry-variant-test-common.el ends here

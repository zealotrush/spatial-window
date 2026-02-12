;;; spatial-window-geometry-hungarian.el --- Hungarian-seeded mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang <lewang.dev.26@gmail.com>
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.9.3
;; Keywords: convenience, windows

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Spatial mapping using a global assignment (Hungarian) to guarantee
;; one distinct key per window, then margin-based fill for the rest.

;;; Code:

(require 'cl-lib)

(defvar spatial-window-keyboard-layout)
(declare-function spatial-window--get-layout "spatial-window")

(defconst spatial-window--assignment-margin 0.05
  "Minimum overlap advantage for a cell to be assigned.")

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

(defun spatial-window--window-bounds (&optional frame)
  "Return window bounds for FRAME as list of (window x-start x-end y-start y-end)."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (float (frame-pixel-width frame)))
         (frame-h (float (frame-pixel-height frame)))
         (windows (window-list frame 'no-minibuf)))
    (mapcar (lambda (w)
              (let ((edges (window-pixel-edges w)))
                (list w
                      (/ (nth 0 edges) frame-w)
                      (/ (nth 2 edges) frame-w)
                      (/ (nth 1 edges) frame-h)
                      (/ (nth 3 edges) frame-h))))
            windows)))

(defun spatial-window--cell-overlap (cell-row cell-col kbd-rows kbd-cols
                                              win-x-start win-x-end win-y-start win-y-end)
  "Return overlap fraction between a cell and a window."
  (let* ((cell-x-start (/ (float cell-col) kbd-cols))
         (cell-x-end (/ (float (1+ cell-col)) kbd-cols))
         (cell-y-start (/ (float cell-row) kbd-rows))
         (cell-y-end (/ (float (1+ cell-row)) kbd-rows))
         (x-overlap-start (max cell-x-start win-x-start))
         (x-overlap-end (min cell-x-end win-x-end))
         (y-overlap-start (max cell-y-start win-y-start))
         (y-overlap-end (min cell-y-end win-y-end))
         (x-overlap-size (max 0.0 (- x-overlap-end x-overlap-start)))
         (y-overlap-size (max 0.0 (- y-overlap-end y-overlap-start)))
         (overlap-area (* x-overlap-size y-overlap-size))
         (cell-width (/ 1.0 kbd-cols))
         (cell-height (/ 1.0 kbd-rows))
         (cell-area (* cell-width cell-height)))
    (/ overlap-area cell-area)))

(defun spatial-window--hungarian (cost)
  "Solve rectangular assignment with Hungarian algorithm.
COST is a list of lists with N rows and M cols, N <= M. Returns alist
of (row-index . col-index), 0-based."
  (let* ((n (length cost))
         (m (length (car cost)))
         (u (make-vector (1+ n) 0.0))
         (v (make-vector (1+ m) 0.0))
         (p (make-vector (1+ m) 0))
         (way (make-vector (1+ m) 0)))
    (dotimes (i n)
      (aset p 0 (1+ i))
      (let ((j0 0)
            (minv (make-vector (1+ m) most-positive-fixnum))
            (used (make-vector (1+ m) nil)))
        (while t
          (aset used j0 t)
          (let* ((i0 (aref p j0))
                 (delta most-positive-fixnum)
                 (j1 0))
            (dotimes (j m)
              (unless (aref used (1+ j))
                (let* ((cur (- (nth j (nth (1- i0) cost))
                               (aref u i0)
                               (aref v (1+ j)))))
                  (when (< cur (aref minv (1+ j)))
                    (aset minv (1+ j) cur)
                    (aset way (1+ j) j0))
                  (when (< (aref minv (1+ j)) delta)
                    (setq delta (aref minv (1+ j)))
                    (setq j1 (1+ j))))))
            (dotimes (j (1+ m))
              (if (aref used j)
                  (progn
                    (aset u (aref p j) (+ (aref u (aref p j)) delta))
                    (aset v j (- (aref v j) delta)))
                (aset minv j (- (aref minv j) delta))))
            (setq j0 j1)
            (when (= (aref p j0) 0)
              (while t
                (let ((j1 (aref way j0)))
                  (aset p j0 (aref p j1))
                  (setq j0 j1))
                (when (= j0 0) (cl-return)))
              (cl-return))))))
    (let ((result nil))
      (dotimes (j m)
        (let ((i (aref p (1+ j))))
          (when (> i 0)
            (push (cons (1- i) j) result))))
      result)))

(defun spatial-window--assign-cells (kbd-rows kbd-cols window-bounds)
  "Assign each cell in grid to best window by margin."
  (let ((grid (make-vector kbd-rows nil)))
    (dotimes (row kbd-rows)
      (aset grid row (make-vector kbd-cols nil))
      (dotimes (col kbd-cols)
        (let ((best-win nil) (best-ov 0.0) (second-ov 0.0))
          (dolist (wb window-bounds)
            (let* ((ov (spatial-window--cell-overlap
                        row col kbd-rows kbd-cols
                        (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb))))
              (cond
               ((> ov best-ov)
                (setq second-ov best-ov best-ov ov best-win (car wb)))
               ((> ov second-ov)
                (setq second-ov ov)))))
          (when (and best-win
                     (> (- best-ov second-ov) spatial-window--assignment-margin))
            (aset (aref grid row) col best-win)))))
    grid))

(defun spatial-window--count-all-keys (final kbd-rows kbd-cols)
  "Count keys per window in FINAL grid of KBD-ROWS x KBD-COLS."
  (let ((counts (make-hash-table :test 'eq)))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref final row) col)))
          (when win
            (puthash win (1+ (gethash win counts 0)) counts)))))
    counts))

(defun spatial-window--seed-unique-keys (final kbd-rows kbd-cols window-bounds)
  "Use Hungarian assignment to give each window a unique best-overlap cell."
  (let* ((cells (cl-loop for r below kbd-rows
                         append (cl-loop for c below kbd-cols
                                         collect (cons r c))))
         (cost (mapcar (lambda (wb)
                         (mapcar (lambda (cell)
                                   (- 1.0 (spatial-window--cell-overlap
                                           (car cell) (cdr cell)
                                           kbd-rows kbd-cols
                                           (nth 1 wb) (nth 2 wb)
                                           (nth 3 wb) (nth 4 wb))))
                                 cells))
                       window-bounds))
         (assignment (spatial-window--hungarian cost)))
    (dolist (pair assignment)
      (let* ((row-idx (car pair))
             (cell-idx (cdr pair))
             (win (car (nth row-idx window-bounds)))
             (cell (nth cell-idx cells))
             (r (car cell))
             (c (cdr cell)))
        (aset (aref final r) c win)))))

(defun spatial-window--ensure-all-windows-have-keys (final kbd-rows kbd-cols window-bounds)
  "Ensure every window gets at least one key (Hungarian seeding)."
  (spatial-window--seed-unique-keys final kbd-rows kbd-cols window-bounds)
  (let ((counts (spatial-window--count-all-keys final kbd-rows kbd-cols)))
    (dolist (wb window-bounds)
      (let ((win (car wb)))
        (when (= (gethash win counts 0) 0)
          (puthash win 1 counts))))))

(defun spatial-window--final-to-keys (final kbd-rows kbd-cols kbd-layout)
  "Convert FINAL assignment grid to alist of (window . keys)."
  (let ((result (make-hash-table :test 'eq)))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref final row) col)))
          (when win
            (push (nth col (nth row kbd-layout)) (gethash win result))))))
    (let ((alist nil))
      (maphash (lambda (win keys)
                 (push (cons win (nreverse keys)) alist))
               result)
      alist)))

(defun spatial-window--assign-keys (&optional frame window-bounds kbd-layout)
  "Assign keyboard keys to windows based on spatial overlap."
  (let ((kbd-layout (or kbd-layout (spatial-window--get-layout))))
    (if (not (apply #'= (mapcar #'length kbd-layout)))
        (progn
          (message "Invalid keyboard layout: rows have different lengths")
          nil)
      (let* ((window-bounds (or window-bounds (spatial-window--window-bounds frame)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             (num-windows (length window-bounds)))
        (if (> num-windows (* kbd-rows kbd-cols))
            (progn
              (message "Too many windows: %d windows for %d keys" num-windows (* kbd-rows kbd-cols))
              nil)
          (let ((final (spatial-window--assign-cells kbd-rows kbd-cols window-bounds)))
            (spatial-window--ensure-all-windows-have-keys
             final kbd-rows kbd-cols window-bounds)
            (spatial-window--final-to-keys final kbd-rows kbd-cols kbd-layout)))))))

(defun spatial-window--format-key-grid (keys)
  "Format KEYS as a keyboard grid string."
  (let ((key-set (make-hash-table :test 'equal)))
    (dolist (k keys)
      (puthash k t key-set))
    (mapconcat
     (lambda (row)
       (mapconcat
        (lambda (key)
          (if (gethash key key-set) key "·"))
        row " "))
     (spatial-window--get-layout)
     "\n")))

(provide 'spatial-window-geometry-hungarian)

;;; spatial-window-geometry-hungarian.el ends here

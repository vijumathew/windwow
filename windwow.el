;;; windwow.el --- simple workspace management

;; Copyright (C) 2017 Viju Mathew

;; Author: Viju Mathew <viju.jm@gmail.com>]
;; Created: 12 May 2017
;; Package-Requires: ((dash "2.13.0") (cl-lib "0.6.1"))
;; Keywords: frames
;; Homepage: github.com/vijumathew/windwow

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a small collection of functions for saving and
;; loading window and buffer configurations. A buffer configuration is
;; a list of buffers and a window configuration is an arrangement of windows
;; in a frame. Right now window configurations created only with split
;; and switch commands are supported. These functions can be called interactively
;; (via `M-x`) or from keybindings.

;; ## Functions
;; ### Buffer
;;  - `save-buffer-list` - saves current buffers and prompts for name
;;  - `load-buffer-list` - loads a previously saved buffer list
;;  - `load-buffer-from-list` - loads a buffer from a saved buffer list
;;
;; ### Window
;;  - `save-window-configuration` - saves current window configuration
;;  - `load-window-configuration` - loads a previously saved window configuration
;;
;; ### Buffer and window
;;  - `load-window-configuration-and-buffer-list` - loads a window configuration and a buffer list 

;;; Code:

(require 'dash)
(require 'cl-lib)

(defvar list-of-buffer-lists '())
(defvar list-of-window-commands '())
(defvar buffer-persistence-file-name)
(defvar window-persistence-file-name)

(setq buffer-persistence-file-name
  (expand-file-name "windwow-persist-buffer.eld"
                    user-emacs-directory))
(setq window-persistence-file-name
  (expand-file-name "windwow-persist-window.eld"
                    user-emacs-directory))

;; persisting the data structures -> based on projectile.el
(defun save-to-file (data filename)
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun read-from-file (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defun init-vars ()
  (setq list-of-buffer-lists
        (read-from-file buffer-persistence-file-name))
  (setq list-of-window-configs
        (read-from-file window-persistence-file-name)))

(defun persist-vars ()
  (save-to-file list-of-buffer-lists
                buffer-persistence-file-name)
  (save-to-file list-of-window-configs
                window-persistence-file-name))

(init-vars)
(add-hook 'kill-emacs-hook 'persist-vars)

;; buffer stuff
(defun get-buffer-list ()
  (cl-mapcar (lambda (window)
               (buffer-name (window-buffer window)))
             (window-list)))

(defun -load-buffer-list (buffers)
  (cl-mapcar (lambda (buffer window)
               (window--display-buffer (get-buffer buffer)
                                       window 'window))
             buffers (window-list)))

(defun get-buffer-list-name (buffers)
  (mapconcat 'identity buffers " "))
 
;; buffer functions to bind
;;;###autoload
(defun save-buffer-list (name)
  "saves current buffers and prompts for name"
  (interactive
   (list (completing-read "Enter buffer list name: "
                          list-of-buffer-lists)))
  (let ((buffer-list (get-buffer-list)))
    (--save-buffer-list name buffer-list)))

(defun -save-buffer-list ()
  (let ((buffer-list (get-buffer-list)))
    (--save-buffer-list (get-buffer-list-name buffer-list) buffer-list)))

(defun --save-buffer-list (name buffers)
  (setf list-of-buffer-lists
        (cons (cons name buffers) list-of-buffer-lists)))

;;;###autoload
(defun load-buffer-list (prompt)
  "loads a previously saved buffer list"
  (interactive
   (list (completing-read "Load buffer list: "
                          list-of-buffer-lists
                          nil t "")))
  (-load-buffer-list (cdr (assoc prompt list-of-buffer-lists))))

;;;###autoload
(defun load-buffer-from-list (buffer-list buffer)
  "loads a buffer from a saved buffer list"
  (interactive
   (let* ((list-name (completing-read "choose buffer-list: " list-of-buffer-lists))
          (b-cur (completing-read "choose buffer: " (cdr (assoc list-name
                                                                list-of-buffer-lists)))))
      (list list-name b-cur)))
  (switch-to-buffer buffer))

;; window stuff
(defun current-frame-data ()
  (let ((parent (frame-root-window)))
    (let ((horiz-frame (window-total-width parent))
          (vert-frame (window-total-height parent))
          (horiz-dimens (-map 'window-total-width (window-list nil nil (frame-first-window))))
          (vert-dimens (-map 'window-total-height (window-list nil nil (frame-first-window)))))
      (list horiz-frame vert-frame horiz-dimens vert-dimens))))

(defun get-split-window-commands (window-config)
  (get-split-window-commands-recur window-config nil nil))

(defun get-split-window-commands-recur (window-config matches commands)
    ;; more than one window
  (if (cdar (cddr window-config))
      (let ((matches (get-possible-splits window-config)))
        (cl-loop for match in matches do
                 (let ((new-set (remove-from-list match matches))
                       (direction (car match))
                       (new-window-config (add-to-config (create-new-window match)
                                                         (remove-from-config match window-config))))
                   ;; check for valid window-config here?
                   (let ((result (get-split-window-commands-recur
                                  new-window-config
                                  new-set
                                  (cons match commands))))
                     (when result
                       (cl-return result))))))
      commands))

(defun add-to-config (window-pair config)
  (let ((first-list (cl-caddr config))
        (second-list (cl-cadddr config)))
    (list (car config) (cadr config)
          (cons (car window-pair) first-list)
          (cons (cdr window-pair) second-list))))

(defun unzip-cons-cells (cells)
  (let ((temp (-reduce-from (lambda (memo item)
                              (list (cons (car item) (car memo))
                                    (cons (cdr item) (cadr memo)))) '(nil nil) cells)))
    (list (reverse (car temp)) (reverse (cadr temp)))))

(defun remove-from-config (split-bundle config)
  (let ((first-list (cl-caddr config))
        (second-list (cl-cadddr config))
        (h-vals (cadr split-bundle))
        (v-vals (cl-caddr split-bundle)))
    (let ((merged-config (-zip first-list second-list))
          (merged (list (cons (car h-vals) (car v-vals))
                        (cons (cdr h-vals) (cdr v-vals)))))
      (let ((removed (unzip-cons-cells (remove-from-list (cadr merged)
                                                         (remove-from-list (car merged) merged-config)))))
        (cons (car config) (cons (cadr config) removed))))))

(defun create-new-window (split-bundle)
  (if (eql 'vertical (car split-bundle))
      (let ((sum-cell (cadr split-bundle)))
        (cons (+ (car sum-cell) (cdr sum-cell))
              (cl-caaddr split-bundle)))
    (let ((sum-cell (cl-caddr split-bundle)))
      (cons (cl-caadr split-bundle)
            (+ (car sum-cell) (cdr sum-cell))))))

;; add in max value filtering (don't use it if it's impossibly tall)
(defun get-possible-splits (window-config)
  "return list of splits and windows from window-config"
  (let ((windows (cddr window-config))
        (h-max (car window-config))
        (v-max (cadr window-config)))
    (let ((horiz (car windows))
          (vert (cadr windows)))
      (get-possible-splits-recur horiz vert h-max v-max nil))))

(defun get-possible-splits-recur (horiz vert h-max v-max directions-and-windows)
  (if horiz
      (let ((h-matches (get-match-indices (car horiz) (cdr horiz) vert v-max))
            (v-matches (get-match-indices (car vert) (cdr vert) horiz h-max)))
        (let ((h-set (build-sets h-matches horiz vert 'horizontal))
              (v-set (build-sets v-matches horiz vert 'vertical)))
          (get-possible-splits-recur (cdr horiz) (cdr vert) h-max v-max
                                     (append directions-and-windows h-set v-set))))
    directions-and-windows))

(defun get-match-indices (elem coll compare-coll compare-max)
  (get-match-indices-recur elem coll 0 '() compare-coll compare-max))

(defun get-match-indices-recur (elem coll index indices compare-coll compare-max)
  (if coll
      (get-match-indices-recur elem (cdr coll) (+ 1 index)
                               (if (let ((compare-elem-1 (car compare-coll))
                                         (compare-elem-2 (nth (+ 1 index) compare-coll)))
                                     (and (equal elem (car coll))
                                          (<= (+ compare-elem-1 compare-elem-2) compare-max)
                                          (<= (abs (- compare-elem-1 compare-elem-2))
                                              2))) ;; split window only
                                   (cons index indices)
                                 indices)
                               compare-coll compare-max)
    indices))

(defun ordered-cons-cell (v1 v2)
  (cons (max v1 v2)
        (min v1 v2)))

;; return value '((split-direction '(h1 h2) (v1 v2)) ... )
(defun build-sets (indices h-set v-set direction)
  (-map (lambda (index)
          (let ((h-val (nth (+ 1 index) h-set))
                (v-val (nth (+ 1 index) v-set)))
            (list direction (ordered-cons-cell (car h-set) h-val)
                  (ordered-cons-cell (car v-set) v-val))))
        indices))

(defun remove-from-list (my-val list)
  (-remove-first (lambda (x) (equal my-val x)) list))

(defun largest-val (l)
  (if l
      (largest-val-recur l 0 0 (car l))
    nil))

(defun largest-val-recur (l index index-return current)
  (if l
      (let ((check (> (car l) current)))
        (largest-val-recur (cdr l) (+ 1 index)
                           (if check index index-return)
                           (if check (car l) current)))
    (cons current index-return)))

;; recreate window commands with splits
(defun get-usable-commands (window-config)
  (let ((commands (get-split-window-commands window-config)))
    (get-switch-and-split-commands commands window-config)))

(defun get-switch-and-split-commands (matches window-config)
  "matches are '(direction (h . h) (v . v))"
  (reverse (-parse-matches matches window-config)))

(defun -parse-matches (matches window-config)
  "matches are '(direction (h . h) (v . v))"
  (parse-matches-recur matches nil 0 (list (cons (car window-config)
                                                 (cadr window-config)))))

(defun parse-matches-recur (matches commands index window-list)
  (if (is-empty matches)
      commands
    (let* ((current (nth index window-list))
           (match (car matches))
           (command (car match))
           (pair (split-window-pair-in-direction command current)))
      (if (equal pair (merge-pair (cdr match)))
          (parse-matches-recur (cdr matches)
                               (cons command commands)
                               index
                               (insert-split-window-at-index index pair window-list))
        (parse-matches-recur matches
                             (cons 'switch commands)
                             (increment-window-index index window-list)
                             window-list)))))

(defun merge-pair (cells)
  (let ((cell-1 (car cells))
        (cell-2 (cadr cells)))
    (list (cons (car cell-1)
                (car cell-2))
          (cons (cdr cell-1)
                (cdr cell-2)))))

(defun increment-window-index (index window-list)
  "increments with mod"
  (mod (+ 1 index) (length window-list)))

(defun insert-split-window-at-index (index window-pair window-list)
  (-insert-at index (car window-pair)
              (-replace-at index (cadr window-pair) window-list)))

(defun split-window-pair-in-direction (direction window)
  (if (eq direction 'vertical)
      (let ((splitted (split-dimension (car window))))
        (list (cons (car splitted) (cdr window))
              (cons (cdr splitted) (cdr window))))
    (let ((splitted (split-dimension (cdr window))))
      (list (cons (car window) (car splitted))
            (cons (car window) (cdr splitted))))))

(defun split-dimension (dimension)
  (if (is-even dimension)
      (let ((half (/ dimension 2)))
        (cons half half))
    (let ((bigger-half (/ (+ dimension 1) 2)))
      (cons bigger-half
            (- bigger-half 1)))))

(defun is-even (number)
  (= (% number 2) 0))

(defun is-empty (dis-list)
  (if (and (listp dis-list)
           (car dis-list))
      nil
    t))

;; execute functions
(defun -load-window-configuration (commands)
  (delete-other-windows)
  (execute-split-window-commands commands))

(defun execute-split-window-commands (commands)
  (-each commands (lambda (command)
                    (cond ((eql command 'vertical)
                           (split-window nil nil 'right))
                          ((eql command 'horizontal)
                           (split-window nil nil 'below))
                          ((eql command 'switch)
                           (other-window 1))))))

;; window functions to bind
;;;###autoload
(defun save-window-configuration (name)
  "saves current window configuration"
  (interactive
   (list (completing-read "Enter split window command list name: "
                          list-of-window-commands)))
  (let ((window-commands (get-usable-commands (current-frame-data))))
    (--save-window-configuration name window-commands)))

(defun --save-window-configuration (name window-commands)
  (setf list-of-window-commands
        (cons (cons name window-commands) list-of-window-commands)))

;;;###autoload
(defun load-window-configuration (prompt)
  "loads a previously saved window configuration"
  (interactive
   (list (completing-read "Load split window command list: "
                          list-of-window-commands
                          nil t "")))
  (-load-window-configuration (cdr (assoc prompt list-of-window-commands))))

;; buffer and window functions
;;;###autoload
(defun load-window-configuration-and-buffer-list (commands buffers)
  "loads a window configuration and a buffer list"
  (interactive
   (let* ((split-commands-name (completing-read "choose window commands: " list-of-window-commands nil t ""))
          (list-name (completing-read "choose buffer-list: " list-of-buffer-lists nil t "")))
     (list (assoc split-commands-name
                  list-of-window-commands)
           (assoc list-name
                  list-of-buffer-lists))))
  (-load-window-configuration commands)
  (-load-buffer-list buffers))

;;; windwow.el ends here

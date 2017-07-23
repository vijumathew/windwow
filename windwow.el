;;; windwow.el --- simple workspace management

;; Copyright (C) 2017 Viju Mathew

;; Author: Viju Mathew <viju.jm@gmail.com>
;; Version: 0.1
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
;;  - `windwow-save-buffer-list` - saves current buffers and prompts for name
;;  - `windwow-load-buffer-list` - loads a previously saved buffer list
;;  - `windwow-load-buffer-from-list` - loads a buffer from a saved buffer list
;;
;; ### Window
;;  - `windwow-save-window-configuration` - saves current window configuration
;;  - `windwow-load-window-configuration` - loads a previously saved window configuration
;;
;; ### Buffer and window
;;  - `windwow-load-window-configuration-and-buffer-list` - loads a window configuration and a buffer list

;;; Code:
(require 'dash)
(require 'cl-lib)

(defvar windwow-list-of-buffer-lists '())
(defvar windwow-list-of-window-commands '())
(defvar windwow-buffer-persistence-file-name)
(defvar windwow-window-persistence-file-name)

(setq windwow-buffer-persistence-file-name
      (expand-file-name "windwow-persist-buffer.eld"
                        user-emacs-directory))

(setq windwow-window-persistence-file-name
      (expand-file-name "windwow-persist-window.eld"
                        user-emacs-directory))

;; persisting the data structures -> based on projectile.el
(defun windwow-save-to-file (data filename)
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun windwow-read-from-file (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defun windwow-init-vars ()
  (setq windwow-list-of-buffer-lists
        (windwow-read-from-file windwow-buffer-persistence-file-name))
  (setq windwow-list-of-window-commands
        (windwow-read-from-file windwow-window-persistence-file-name)))

(defun windwow-persist-vars-function ()
  (windwow-save-to-file windwow-list-of-buffer-lists
                        windwow-buffer-persistence-file-name)
  (windwow-save-to-file windwow-list-of-window-commands
                        windwow-window-persistence-file-name))

(windwow-init-vars)
(add-hook 'kill-emacs-hook 'windwow-persist-vars-function)

(defun windwow-unload-function ()
  (remove-hook 'kill-emacs-hook 'windwow-persist-vars-function))

;; buffer stuff
(defun windwow-get-buffer-list ()
  (cl-mapcar (lambda (window)
               (buffer-name (window-buffer window)))
             (window-list nil nil (frame-first-window))))

(defun windwow-load-buffer-list-buffers (buffers)
  (cl-mapcar (lambda (buffer window)
               (window--display-buffer (get-buffer buffer)
                                       window 'window))
             buffers (window-list nil nil (frame-first-window))))

(defun windwow-get-buffer-list-name (buffers)
  (mapconcat 'identity buffers " "))

;; buffer functions to bind
;;;###autoload
(defun windwow-save-buffer-list (name)
  "saves current buffers and prompts for name"
  (interactive
   (list (completing-read "Enter buffer list name: "
                          windwow-list-of-buffer-lists)))
  (let ((buffer-list (windwow-get-buffer-list)))
    (windwow-save-buffer-list-args name buffer-list)))

;;;###autoload
(defun windwow-save-buffer-list-no-name ()
  "saves current buffers as names of all buffers"
  (interactive)
  (let ((buffer-list (windwow-get-buffer-list)))
    (windwow-save-buffer-list-args (windwow-get-buffer-list-name buffer-list)
                                   buffer-list)))

(defun windwow-save-buffer-list-args (name buffers)
  (setf windwow-list-of-buffer-lists 
        (cons (cons name buffers) windwow-list-of-buffer-lists)))

;;;###autoload
(defun windwow-load-buffer-list (prompt)
  "loads a previously saved buffer list"
  (interactive
   (list (completing-read "Load buffer list: "
                          windwow-list-of-buffer-lists
                          nil t "")))
  (windwow-load-buffer-list-buffers (cdr (assoc prompt windwow-list-of-buffer-lists))))

;;;###autoload
(defun windwow-load-buffer-from-list (buffer-list buffer)
  "loads a buffer from a saved buffer list"
  (interactive
   (let* ((list-name (completing-read "choose buffer-list: " windwow-list-of-buffer-lists))
          (b-cur (completing-read "choose buffer: " (cdr (assoc list-name
                                                                windwow-list-of-buffer-lists)))))
     (list list-name b-cur)))
  (switch-to-buffer buffer))

;; window stuff
(defun windwow-current-frame-data ()
  (let ((parent (frame-root-window)))
    (let ((horiz-frame (window-total-width parent))
          (vert-frame (window-total-height parent))
          (horiz-dimens (-map 'window-total-width (window-list nil nil (frame-first-window))))
          (vert-dimens (-map 'window-total-height (window-list nil nil (frame-first-window)))))
      (list horiz-frame vert-frame horiz-dimens vert-dimens))))

(defun windwow-get-split-window-commands (window-config)
  (windwow-get-split-window-commands-recur window-config nil nil))

(defun windwow-get-split-window-commands-recur (window-config matches commands)
  ;; more than one window
  (if (cdar (cddr window-config))
      (let ((matches (windwow-get-possible-splits window-config)))
        (cl-loop for match in matches do
                 (let ((new-set (windwow-remove-from-list match matches))
                       (direction (car match))
                       (new-window-config (windwow-add-to-config (windwow-create-new-window match)
                                                                 (windwow-remove-from-config
                                                                  match window-config))))
                   ;; check for valid window-config here?
                   (let ((result (windwow-get-split-window-commands-recur
                                  new-window-config
                                  new-set
                                  (cons match commands))))
                     (when result
                       (cl-return result))))))
    commands))

(defun windwow-add-to-config (window-pair config)
  (let ((first-list (cl-caddr config))
        (second-list (cl-cadddr config)))
    (list (car config) (cadr config)
          (cons (car window-pair) first-list)
          (cons (cdr window-pair) second-list))))

(defun windwow-unzip-cons-cells (cells)
  (let ((temp (-reduce-from (lambda (memo item)
                              (list (cons (car item) (car memo))
                                    (cons (cdr item) (cadr memo)))) '(nil nil) cells)))
    (list (reverse (car temp)) (reverse (cadr temp)))))

(defun windwow-remove-from-config (split-bundle config)
  (let ((first-list (cl-caddr config))
        (second-list (cl-cadddr config))
        (h-vals (cadr split-bundle))
        (v-vals (cl-caddr split-bundle)))
    (let ((merged-config (-zip first-list second-list))
          (merged (list (cons (car h-vals) (car v-vals))
                        (cons (cdr h-vals) (cdr v-vals)))))
      (let ((removed (windwow-unzip-cons-cells
                      (windwow-remove-from-list (cadr merged)
                                                (windwow-remove-from-list (car merged) merged-config)))))
        (cons (car config) (cons (cadr config) removed))))))

(defun windwow-create-new-window (split-bundle)
  (if (eql 'vertical (car split-bundle))
      (let ((sum-cell (cadr split-bundle)))
        (cons (+ (car sum-cell) (cdr sum-cell))
              (cl-caaddr split-bundle)))
    (let ((sum-cell (cl-caddr split-bundle)))
      (cons (cl-caadr split-bundle)
            (+ (car sum-cell) (cdr sum-cell))))))

;; add in max value filtering (don't use it if it's impossibly tall)
(defun windwow-get-possible-splits (window-config)
  "return list of splits and windows from window-config"
  (let ((windows (cddr window-config))
        (h-max (car window-config))
        (v-max (cadr window-config)))
    (let ((horiz (car windows))
          (vert (cadr windows)))
      (windwow-get-possible-splits-recur horiz vert h-max v-max nil))))

(defun windwow-get-possible-splits-recur (horiz vert h-max v-max directions-and-windows)
  (if horiz
      (let ((h-matches (windwow-get-match-indices (car horiz) (cdr horiz) vert v-max))
            (v-matches (windwow-get-match-indices (car vert) (cdr vert) horiz h-max)))
        (let ((h-set (windwow-build-sets h-matches horiz vert 'horizontal))
              (v-set (windwow-build-sets v-matches horiz vert 'vertical)))
          (windwow-get-possible-splits-recur (cdr horiz) (cdr vert) h-max v-max
                                             (append directions-and-windows h-set v-set))))
    directions-and-windows))

(defun windwow-get-match-indices (elem coll compare-coll compare-max)
  (windwow-get-match-indices-recur elem coll 0 '() compare-coll compare-max))

(defun windwow-get-match-indices-recur (elem coll index indices compare-coll compare-max)
  (if coll
      (windwow-get-match-indices-recur elem (cdr coll) (+ 1 index)
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

(defun windwow-ordered-cons-cell (v1 v2)
  (cons (max v1 v2)
        (min v1 v2)))

;; return value '((split-direction '(h1 h2) (v1 v2)) ... )
(defun windwow-build-sets (indices h-set v-set direction)
  (-map (lambda (index)
          (let ((h-val (nth (+ 1 index) h-set))
                (v-val (nth (+ 1 index) v-set)))
            (list direction (windwow-ordered-cons-cell (car h-set) h-val)
                  (windwow-ordered-cons-cell (car v-set) v-val))))
        indices))

(defun windwow-remove-from-list (my-val list)
  (-remove-first (lambda (x) (equal my-val x)) list))

;; recreate window commands with splits
(defun windwow-get-usable-commands (window-config)
  (let ((commands (windwow-get-split-window-commands window-config)))
    (windwow-get-switch-and-split-commands commands window-config)))

(defun windwow-get-switch-and-split-commands (matches window-config)
  "matches are '(direction (h . h) (v . v))"
  (reverse (windwow-parse-matches matches window-config)))

(defun windwow-parse-matches (matches window-config)
  "matches are '(direction (h . h) (v . v))"
  (windwow-parse-matches-recur matches nil 0 (list (cons (car window-config)
                                                         (cadr window-config)))))

(defun windwow-parse-matches-recur (matches commands index window-list)
  (if (windwow-is-empty matches)
      commands
    (let* ((current (nth index window-list))
           (match (car matches))
           (command (car match))
           (pair (windwow-split-window-pair-in-direction command current)))
      (if (equal pair (windwow-merge-pair (cdr match)))
          (windwow-parse-matches-recur (cdr matches)
                                       (cons command commands)
                                       index
                                       (windwow-insert-split-window-at-index index pair window-list))
        (windwow-parse-matches-recur matches
                                     (cons 'switch commands)
                                     (windwow-increment-window-index index window-list)
                                     window-list)))))

(defun windwow-merge-pair (cells)
  (let ((cell-1 (car cells))
        (cell-2 (cadr cells)))
    (list (cons (car cell-1)
                (car cell-2))
          (cons (cdr cell-1)
                (cdr cell-2)))))

(defun windwow-increment-window-index (index window-list)
  "increments with mod"
  (mod (+ 1 index) (length window-list)))

(defun windwow-insert-split-window-at-index (index window-pair window-list)
  (-insert-at index (car window-pair)
              (-replace-at index (cadr window-pair) window-list)))

(defun windwow-split-window-pair-in-direction (direction window)
  (if (eq direction 'vertical)
      (let ((splitted (windwow-split-dimension (car window))))
        (list (cons (car splitted) (cdr window))
              (cons (cdr splitted) (cdr window))))
    (let ((splitted (windwow-split-dimension (cdr window))))
      (list (cons (car window) (car splitted))
            (cons (car window) (cdr splitted))))))

(defun windwow-split-dimension (dimension)
  (if (windwow-is-even dimension)
      (let ((half (/ dimension 2)))
        (cons half half))
    (let ((bigger-half (/ (+ dimension 1) 2)))
      (cons bigger-half
            (- bigger-half 1)))))

(defun windwow-is-even (number)
  (= (% number 2) 0))

(defun windwow-is-empty (dis-list)
  (if (and (listp dis-list)
           (car dis-list))
      nil
    t))

;; execute functions
(defun windwow-load-window-configuration-commands (commands)
  (let ((buffers (windwow-get-buffer-list)))
    (delete-other-windows)
    (windwow-execute-split-window-commands commands)
    (windwow-load-buffer-list-buffers buffers)))

(defun windwow-execute-split-window-commands (commands)
  (-each commands (lambda (command)
                    (cond ((eql command 'vertical)
                           (split-window nil nil 'right))
                          ((eql command 'horizontal)
                           (split-window nil nil 'below))
                          ((eql command 'switch)
                           (other-window 1))))))

;; window functions to bind
;;;###autoload
(defun windwow-save-window-configuration (name)
  "saves current window configuration"
  (interactive
   (list (completing-read "Enter split window command list name: "
                          windwow-list-of-window-commands)))
  (let ((window-commands (windwow-get-usable-commands (windwow-current-frame-data))))
    (windwow-save-window-configuration-commands name window-commands)))

(defun windwow-save-window-configuration-commands (name window-commands)
  (setf windwow-list-of-window-commands
        (cons (cons name window-commands) windwow-list-of-window-commands)))

;;;###autoload
(defun windwow-load-window-configuration (prompt)
  "loads a previously saved window configuration"
  (interactive
   (list (completing-read "Load split window command list: "
                          windwow-list-of-window-commands
                          nil t "")))
  (windwow-load-window-configuration-commands (cdr (assoc prompt windwow-list-of-window-commands))))

;; buffer and window functions
;;;###autoload
(defun windwow-load-window-configuration-and-buffer-list (commands buffers)
  "loads a window configuration and a buffer list"
  (interactive
   (let* ((split-commands-name (completing-read "choose window commands: "
                                                windwow-list-of-window-commands nil t ""))
          (list-name (completing-read "choose buffer-list: " windwow-list-of-buffer-lists nil t "")))
     (list (assoc split-commands-name
                  windwow-list-of-window-commands)
           (assoc list-name
                  windwow-list-of-buffer-lists))))
  (windwow-load-window-configuration-commands commands)
  (windwow-load-buffer-list-buffers buffers))

(provide 'windwow)
;;; windwow.el ends here

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
(defun save-buffer-list (name)
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

(defun load-buffer-list (prompt)
  (interactive
   (list (completing-read "Load buffer list: "
                          list-of-buffer-lists
                          nil t "")))
  (-load-buffer-list (cdr (assoc prompt list-of-buffer-lists))))

(defun load-buffer-from-list (buffer-list buffer)
  (interactive
   (let* ((list-name (completing-read "choose buffer-list: " list-of-buffer-lists))
          (b-cur (completing-read "choose buffer: " (cdr (assoc list-name
                                                                list-of-buffer-lists)))))
      (list list-name b-cur)))
  (switch-to-buffer buffer))

;; window stuff
(defun get-top-window-parent ()
  (get-top-window-parent-recur (selected-window)))

(defun get-top-window-parent-recur (window)
  (let ((parent (window-parent window)))
    (if parent
        (get-top-window-parent-recur parent)
      window)))

(defun current-frame-data ()
  (let ((parent (get-top-window-parent)))
    (let ((horiz-frame (window-total-width parent))
          (vert-frame (window-total-height parent))
          (horiz-dimens (-map 'window-total-width (window-list)))
          (vert-dimens (-map 'window-total-height (window-list))))
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
                                  (cons direction commands))))
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

;; window config '(v h '(h1 h2 ...) '(v1 v2 ...))
;; return value '((split-direction '(h1 h2) (v1 v2)) ... )

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
                               (if (and (equal elem (car coll))
                                        (<= (+ (car compare-coll) (nth (+ 1 index) compare-coll))
                                           compare-max))
                                   (cons index indices)
                                 indices)
                               compare-coll compare-max)
    indices))

;; return value '((split-direction '(h1 h2) (v1 v2)) ... )
(defun build-sets (indices h-set v-set direction)
  (-map (lambda (index)
          (let ((h-val (nth (+ 1 index) h-set))
                (v-val (nth (+ 1 index) v-set)))
            (list direction (cons (car h-set) h-val)
                  (cons (car v-set) v-val))))
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

(defun get-switch-and-split-commands (commands window-config)
  (let ((switch-commands (-get-switch-and-split-commands commands
                                                         window-config)))
    (reverse (-drop-while (lambda (command)
                            (eq command 'switch)) switch-commands))))

(defun -get-switch-and-split-commands (commands window-config)
  "commands are in reverse order and have nils"
  (let ((horiz (car window-config))
        (vertical (cadr window-config))
        (h-list (cl-caddr window-config))
        (v-list (cl-cadddr window-config)))
    (get-switch-and-split-commands-recur commands
                                         nil
                                         (list (cons horiz vertical))
                                         0
                                         (-zip-pair h-list v-list))))

(defun get-switch-and-split-commands-recur (commands new-commands window-list index final-list)
  (if (and (is-empty commands)
           (is-empty final-list))
      new-commands
    (let ((current-window (nth index window-list)))
      (if (member current-window final-list)
          (get-switch-and-split-commands-recur commands
                                               (cons 'switch new-commands)
                                               window-list
                                               (+ 1 index)
                                               (remove-from-list current-window final-list))
        (let ((new-window-pair (split-window-pair-in-direction (car commands)
                                                               current-window)))
          (get-switch-and-split-commands-recur (cdr commands)
                                               (cons (car commands) new-commands)
                                               (insert-split-window-at-index index
                                                                             new-window-pair
                                                                             window-list)
                                               index
                                               final-list))))))

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
(defun -load-split-window-commands (commands)
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
(defun save-split-window-commands (name)
  (interactive
   (list (completing-read "Enter split window command list name: "
                          list-of-window-commands)))
  (let ((window-commands (get-usable-commands (current-frame-data))))
    (--save-split-window-commands name window-commands)))

(defun --save-split-window-commands (name window-commands)
  (setf list-of-window-commands
        (cons (cons name window-commands) list-of-window-commands)))

(defun load-split-window-commands (prompt)
  (interactive
   (list (completing-read "Load split window command list: "
                          list-of-window-commands
                          nil t "")))
  (-load-split-window-commands (cdr (assoc prompt list-of-window-commands))))

;; buffer and window functions
(defun load-window-commands-and-buffer-list (commands buffers)
  (interactive
   (let* ((split-commands-name (completing-read "choose window commands: " list-of-window-commands nil t ""))
          (list-name (completing-read "choose buffer-list: " list-of-buffer-lists nil t "")))
     (list (assoc split-commands-name
                  list-of-window-commands)
           (assoc list-name
                  list-of-buffer-lists))))
  (-load-split-window-commands commands)
  (-load-buffer-list buffers))

;; example for frame data structure
(defun current-frame-data ()
  (let ((horiz-frame (frame-total-cols))
        (vert-frame (frame-total-lines))
        (horiz-dimens (-map 'window-total-width (window-list)))
        (vert-dimens (-map 'window-total-height (window-list))))
    (list (- horiz-frame 1) (- vert-frame 1) horiz-dimens vert-dimens)))

(require 'dash)
(require 'cl-lib)

(defvar list-of-buffer-lists '())
(defvar list-of-window-configs '())

;; buffer stuff
(defun save-buffer-group () '())

(defun switch-buffer-group () '())
(defun switch-to-buffer-from-buffer-group () '())

;; possibly use 'buffer-file-name for saving
(defun get-buffer-list ()
  (cl-mapcar (lambda (window)
               (buffer-name (window-buffer window)))
             (window-list)))

(defun -load-buffer-list (buffers)
  (cl-mapcar (lambda (buffer window)
               (window--display-buffer (get-buffer buffer)
                                       window 'window))
             buffers (window-list)))

;;(-load-buffer-list '("*scratch*" "*Messages*"))

(defun get-buffer-list-name (buffers)
  (mapconcat 'identity buffers " "))
 
(get-buffer-list-name (get-buffer-list))

;; check flag for persisting - default is off

;; make sure saving overwrites cons cell with same var name
;; what is best name?
(defun save-buffer-list (name)
  (interactive
   (list (completing-read "Enter buffer list name: "
                          b)))
  (let ((buffer-list (get-buffer-list)))
    (--save-buffer-list name buffer-list)))

(defun -save-buffer-list ()
  (let ((buffer-list (get-buffer-list)))
    (--save-buffer-list (get-buffer-list-name buffer-list) buffer-list)))

(defun --save-buffer-list (name buffers)
  (setf b (cons (cons name buffers) b)))

(defun load-buffer-list (prompt)
  (interactive
   (list (completing-read "Load buffer list: "
                          b
                          nil t "")))
  (-load-buffer-list (cdr (assoc prompt b))))

(defun load-buffer-from-list (buffer-list buffer)
  (interactive
   (let* ((list-name (completing-read "choose buffer-list: " b))
          (b-cur (completing-read "choose buffer: " (cdr (assoc list-name b)))))
      (list list-name b-cur)))
  (switch-to-buffer buffer))

;; window stuff
(defun get-split-window-commands (window-config)
  (get-split-window-commands-recur window-config nil nil))

(get-split-window-commands (current-frame-data))
(current-frame-data)

(get-split-window-commands '(128 33
                                 (64 64)
                                 (33 33)))

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

(get-possible-splits '(128 33 (64 64) (33 33)))

(defun get-possible-splits-recur (horiz vert h-max v-max directions-and-windows)
  (if horiz
      (let ((h-matches (get-match-indices (car horiz) (cdr horiz) vert v-max))
            (v-matches (get-match-indices (car vert) (cdr vert) horiz h-max)))
        (let ((h-set (build-sets h-matches horiz vert 'horizontal))
              (v-set (build-sets v-matches horiz vert 'vertical)))
          (get-possible-splits-recur (cdr horiz) (cdr vert) h-max v-max
                                     (append directions-and-windows h-set v-set))))
    directions-and-windows))

(get-match-indices 33 '(33) '(64 64) 128)

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

(defun merge-sorted-sets (&rest lists)
  (merge-sorted-sets-recur nil lists))

(defun merge-sorted-sets-recur (merged lists)
  (if (car lists)
      (let ((large (largest-val (mapcar 'car lists))))
        (let ((largest-val (car large))
              (largest-index (cdr large)))
          (let ((new-list (cdr (nth largest-index lists)))
                (new-lists (-non-nil
                            (-remove-at largest-index lists))))
            (merge-sorted-sets-recur (cons largest-val merged) (if new-list (cons new-list new-lists) new-lists)))))
  merged))

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

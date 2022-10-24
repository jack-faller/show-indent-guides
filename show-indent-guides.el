;;; show-indent-guides.el --- Provides the command `show-indent-guides-mode'. -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the command `show-indent-guides-mode'. This mode will render guides
;; to indicate levels of indentation in a buffer. These are vertical lines which
;; will be drawn straight down from that level of indent.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'dash)
(require 'text-property-search)
(defvar shig-guide-char ?\x2502 "Character that will be used for guides.")
(defvar shig-idle-time 0.01
  "The duration of the idle timer for redrawing the guides after an edit.")
(defvar shig-insert-idle-time 0.2
  "Like SHIG-IDLE-TIME, but used in evil insert state.")
(defvar-local shig-regular-offset nil
  "Save time rendering by assuming that guides are all this many columns apart.")
(defface shig-guide-face '((t . (:foreground "dim grey"))) "face for indent guides")

(defvar shig--tab-width-string (concat (cl-loop repeat 30 collect ?\s)))
(add-to-list 'text-property-default-nonsticky (cons 'indent-guide t))

(defun shig--put-line-end-guide (n str)
  (unless (< n (window-start))
    (put-text-property 0 (length str) 'face 'shig-guide-face str)
    (put-text-property n (+ n 1) 'indent-guide t)
    (put-text-property n (+ n 1) 'display
                       (concat str (or (get-text-property n 'display)
                                       (buffer-substring n (+ n 1)))))))
(defun shig--put-char-guide (n str)
  (unless (< n (window-start))
    (put-text-property 0 (length str) 'face 'shig-guide-face str)
    (put-text-property n (+ n 1) 'indent-guide t)
    (put-text-property n (+ n 1) 'display str)))
(defun shig--remove-display-guides ()
  (save-excursion
    (setf (point) (point-min))
    (let (match)
      (while (setf match (text-property-search-forward 'indent-guide))
        (remove-list-of-text-properties
         (prop-match-beginning match)
         (prop-match-end match)
         '(display face indnet-guide))))))

(defun shig--inscribe-line (beg end indent-levels &optional extra)
  (cl-loop for i from beg below end
           for char-was-put = nil
           with line-level = 0 and indent-level-index = 0 and char-display-string = nil
           do (if (= (char-after i) ?\t)
                  (let ((width (- tab-width (mod line-level tab-width))))
                    (cl-incf line-level width)
                    (setf char-display-string (substring shig--tab-width-string 0 width)))
                (cl-incf line-level)
                (setf char-display-string (substring " ")))
           do (while (< (aref indent-levels indent-level-index) line-level)
                (setf char-was-put t)
                (aset char-display-string
                      (- (length char-display-string)
                         (- line-level (aref indent-levels indent-level-index)))
                      shig-guide-char)
                (cl-incf indent-level-index))
           when char-was-put do (shig--put-char-guide i char-display-string)
           finally
           (progn
             (cond
              (extra
               (cl-loop for depth = (aref indent-levels indent-level-index)
                        for j from line-level
                        until (> depth extra)
                        collect (if (< j depth) ?\s
                                  (cl-incf indent-level-index)
                                  shig-guide-char)
                        into str
                        do (cl-incf i)
                        finally (shig--put-line-end-guide end (concat str))))
              ((= 0 line-level)
               (aset indent-levels 0 999))
              (t (aset indent-levels indent-level-index line-level)
                 (aset indent-levels (+ indent-level-index 1) 999)))
             (cl-return line-level))))

(defun shig--refresh-guides ()
  (let ((inhibit-modification-hooks t)
        (end (window-end))
        (indent-levels (make-vector 80 999))
        (indent-levels-copy (make-vector 80 999))
        blank-lines prev-length)
   (with-silent-modifications
     (shig--remove-display-guides)
     (save-excursion
       (save-match-data
         (setf (point) end
               (point) (line-end-position)
               end (progn (re-search-forward (rx (not (any space ?\n))) nil t) (point))
               (point) (window-start))
         (if (not shig-regular-offset)
             (re-search-backward (rx bol (not (any space ?\n))) nil t)
           (re-search-backward (rx bol (* space) (group (not (any space ?\n)))) nil t)
           (let ((beg (or (match-beginning 0) (point-min)))
                 (end (or (match-beginning 1) (+ (point-min) 1))))
             (dotimes (i (length indent-levels))
               (aset indent-levels i (* shig-regular-offset (+ i 1))))
             (shig--inscribe-line beg end indent-levels)))
         (while (and (re-search-forward (rx (group bol (* space)) (group (not space))) nil t)
                     (< (point) end))
           (if (string= "\n" (match-string 2)) ;; blank line
               (push (cons (match-beginning 1) (match-end 1))
                     blank-lines)
             (cl-loop for i across indent-levels and j from 0
                      do (aset indent-levels-copy j i)
                      until (= i 999))
             (setf prev-length (shig--inscribe-line (match-beginning 1) (match-end 1) indent-levels))
             (mapc (lambda (args) (shig--inscribe-line (car args) (cdr args) indent-levels-copy prev-length))
                   blank-lines)
             (setf blank-lines '()))
           (setf (point) (match-end 0))))))))

(defvar shig--idle-timer nil)
(defun shig--dispatch ()
  (when shig--idle-timer (cancel-timer shig--idle-timer))
  (--> (if (and (boundp 'evil-state) (eq 'insert evil-state))
           shig-insert-idle-time shig-idle-time)
       (run-with-idle-timer it nil #'shig--refresh-guides)
       (setf shig--idle-timer it)))

;;;###autoload
(define-minor-mode show-indent-guides-mode ""
  :lighter "sh-i-g"
  (if show-indent-guides-mode
      (add-hook 'post-command-hook #'shig--dispatch nil t)
    (remove-hook 'post-command-hook #'shig--dispatch t)
    (shig--remove-display-guides)))

(provide 'show-indent-guides)
;;; show-indent-guides.el ends here

(defvar guide-char ?\x2502)
(defface guide-face '((t . (:foreground "dim grey"))) "face for indent guides")

(defvar tab-width-string (concat (cl-loop repeat 30 collect ?\s)))
(add-to-list 'text-property-default-nonsticky (cons 'indent-guide t))

(defun put-line-end-guide (n str)
  (unless (< n (window-start))
    (put-text-property 0 (length str) 'face 'guide-face str)
    (put-text-property n (+ n 1) 'indent-guide t)
    (put-text-property n (+ n 1) 'display
                       (concat str (or (get-text-property n 'display)
                                       (buffer-substring n (+ n 1)))))))
(defun put-char-guide (n str)
  (unless (< n (window-start))
    (put-text-property 0 (length str) 'face 'guide-face str)
    (put-text-property n (+ n 1) 'indent-guide t)
    (put-text-property n (+ n 1) 'display str)))
(defun remove-display-guides ()
  (save-excursion
    (setf (point) (point-min))
    (let (match)
      (while (setf match (text-property-search-forward 'indent-guide))
        (remove-list-of-text-properties
         (prop-match-beginning match)
         (prop-match-end match)
         '(display face indnet-guide))))))

(defun inscribe-line (beg end indent-levels &optional extra)
  (cl-loop for i from beg below end
           for char-was-put = nil
           with line-level = 0 and indent-level-index = 0 and char-display-string = nil
           do (if (= (char-after i) ?\t)
                  (progn (cl-incf line-level tab-width)
                         (setf char-display-string (substring tab-width-string 0 tab-width)))
                (cl-incf line-level)
                (setf char-display-string (substring " ")))
           do (while (< (aref indent-levels indent-level-index) line-level)
                (setf char-was-put t)
                (aset char-display-string
                      (- (length char-display-string)
                         (- line-level (aref indent-levels indent-level-index)))
                      guide-char)
                (cl-incf indent-level-index))
           when char-was-put do (put-char-guide i char-display-string)
           finally
           (progn
             (cond
              (extra
               (cl-loop for depth = (aref indent-levels indent-level-index)
                        for j from line-level
                        until (> depth extra)
                        collect (if (< j depth) ?\s
                                  (cl-incf indent-level-index)
                                  guide-char)
                        into str
                        do (cl-incf i)
                        finally (put-line-end-guide end (concat str))))
              ((= 0 line-level)
               (aset indent-levels 0 999))
              (t (aset indent-levels indent-level-index line-level)
                 (aset indent-levels (+ indent-level-index 1) 999)))
             (cl-return line-level))))

(defun refresh-guides ()
  (let ((inhibit-modification-hooks t)
        (end (window-end))
        (indent-levels (make-vector 80 999))
        (indent-levels-copy (make-vector 80 999))
        blank-lines prev-length)
   (with-silent-modifications
     (remove-display-guides)
     (save-excursion
       (save-match-data
         (setf (point) end
               (point) (line-end-position)
               end (progn (re-search-forward (rx (not (any space ?\n))) nil t) (point))
               (point) (window-start))
         (if (not guide-regular-offset)
             (re-search-backward (rx bol (not (any space ?\n))) nil t)
           (re-search-backward (rx bol (* space) (group (not (any space ?\n)))) nil t)
           (let ((beg (or (match-beginning 0) (point-min)))
                 (end (or (match-beginning 1) (+ (point-min) 1))))
             (dotimes (i (length indent-levels))
               (aset indent-levels i (* guide-regular-offset (+ i 1))))
             (inscribe-line beg end indent-levels)))
         (while (and (re-search-forward (rx (group bol (* space)) (group (not space))) nil t)
                     (< (point) end))
           (if (string= "\n" (match-string 2)) ;; blank line
               (push (cons (match-beginning 1) (match-end 1))
                     blank-lines)
             (cl-loop for i across indent-levels and j from 0
                      do (aset indent-levels-copy j i)
                      until (= i 999))
             (setf prev-length (inscribe-line (match-beginning 1) (match-end 1) indent-levels))
             (mapc (lambda (args) (inscribe-line (car args) (cdr args) indent-levels-copy prev-length))
                   blank-lines)
             (setf blank-lines '()))
           (setf (point) (match-end 0))))))))

(defvar guide-idle-time 0.01)
(defvar guide-insert-idle-time 0.2)
(defvar-local guide-regular-offset nil)
(defvar guide-idle-timer nil)
(defun guide-dispatch ()
  (when guide-idle-timer (cancel-timer guide-idle-timer))
  (--> (if (eq 'insert evil-state) guide-insert-idle-time guide-idle-time)
       (run-with-idle-timer it nil #'refresh-guides)
       (setf guide-idle-timer it)))
(defun guide-mode ()
  (interactive)
  (if (memq #'guide-dispatch post-command-hook)
      (progn
        (remove-hook 'post-command-hook #'guide-dispatch t)
        (remove-display-guides))
    (add-hook 'post-command-hook #'guide-dispatch nil t)))

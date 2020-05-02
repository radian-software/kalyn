;;; kalyn-mode.el --- Major mode for Kalyn -*- lexical-binding: t -*-

;; Copyright (C) 2020 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 25 Feb 2020
;; Keywords: languages
;; Package-Requires: ((emacs "27"))
;; Version: 0

;;; Commentary:

;; Major mode for editing Kalyn source code.

;;; Code:

(defvar kalyn--declaration-builtins
  '("alias" "class" "data" "defn" "def" "derive" "import" "instance" "public" "with"))

(defvar kalyn--special-form-builtins
  '("case" "lambda" "let"))

(defun kalyn--builtins ()
  (append kalyn--declaration-builtins kalyn--special-form-builtins))

(defun kalyn--font-lock-keywords ()
  (list (list (format "\\_<\\(%s\\)\\_>"
                      (mapconcat
                       #'regexp-quote
                       (kalyn--builtins)
                       "\\|"))
              (list 0 'font-lock-keyword-face))
        (list "\\_<[A-Z].*?\\_>"
              (list 0 'font-lock-type-face))))

(defun kalyn--column-at (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun kalyn--indent-function (indent-point state)
  (cl-block nil
    (unless (nth 1 state)
      (cl-return 0))
    (let* ((list-start (1+ (nth 1 state)))
           (first-sexp-end
            (condition-case _
                (scan-sexps list-start 1)
              (scan-error)))
           (first-sexp-start
            (when first-sexp-end
              (scan-sexps first-sexp-end -1)))
           (second-sexp-end
            (condition-case _
                (scan-sexps list-start 2)
              (scan-error)))
           (second-sexp-start
            (when second-sexp-end
              (scan-sexps second-sexp-end -1)))
           (first-sexp (buffer-substring first-sexp-start first-sexp-end))
           (second-sexp-on-same-line-p
            (and first-sexp-end
                 second-sexp-start
                 (<= (count-lines first-sexp-end second-sexp-start) 1)))
           (square-p (= (char-before list-start) ?\[))
           (starting-column (kalyn--column-at list-start)))
      (cond
       ;; [foo
       ;;  bar]
       (square-p
        starting-column)
       ;; ((foo)
       ;;  bar)
       ;;
       ;; ( foo
       ;;  bar)
       ((not (eq (car (syntax-after list-start)) 3))
        starting-column)
       ;; (let bar
       ;;   baz)
       ((member first-sexp (kalyn--builtins))
        (1+ starting-column))
       ;; (foo bar
       ;;      baz)
       (second-sexp-on-same-line-p
        (kalyn--column-at second-sexp-start))
       ;; (foo
       ;;   bar)
       (t
        (1+ starting-column))))))

;;;###autoload
(define-derived-mode kalyn-mode prog-mode "Kalyn"
  "Major mode for editing Kalyn code."
  (map-char-table
   (lambda (range value)
     (when (eq (car value) 2)
       (set-char-table-range (syntax-table) range (cons 3 (cdr value)))))
   (syntax-table))
  (modify-syntax-entry ?\; "<" (syntax-table))
  (modify-syntax-entry ?\n ">" (syntax-table))
  (setq-local comment-start ";;")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults
              (list #'kalyn--font-lock-keywords))
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'kalyn--indent-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kalyn\\'" . kalyn-mode))

(provide 'kalyn-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; kalyn-mode.el ends here

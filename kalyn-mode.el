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

(defvar kalyn-declaration-builtins
  '("alias" "class" "data" "defn" "def" "derive" "instance" "public" "with"))

(defvar kalyn-special-form-builtins
  '("case" "lambda" "let"))

(defun kalyn-builtins ()
  (append kalyn-declaration-builtins kalyn-special-form-builtins))

(defun kalyn-font-lock-keywords ()
  (list (list (format "\\_<\\(%s\\)\\_>"
                      (mapconcat
                       #'regexp-quote
                       (kalyn-builtins)
                       "\\|"))
              (list 0 'font-lock-keyword-face))
        (list "\\_<[A-Z].*?\\_>"
              (list 0 'font-lock-type-face))))

(defun kalyn-indent-function (indent-point state)
  (cl-block nil
    ;; Go to beginning of innermost containing list. If not inside a
    ;; list, indent to left margin.
    (unless (nth 1 state)
      (cl-return 0))
    (goto-char (nth 1 state))
    (+ 2 (current-column))))

(define-derived-mode kalyn-mode prog-mode "Kalyn"
  "Major mode for editing Kalyn code."
  (modify-syntax-entry ?\; "<" (syntax-table))
  (modify-syntax-entry ?\n ">" (syntax-table))
  (setq-local comment-start ";;")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults
              (list #'kalyn-font-lock-keywords))
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'kalyn-indent-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kalyn\\'" . kalyn-mode))

(provide 'kalyn-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; kalyn-mode.el ends here

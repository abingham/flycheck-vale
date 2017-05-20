;;; flycheck-vale.el --- flycheck integration for vale
;; Copyright (c) 2017 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/flycheck-vale
;; Package-Requires: ((emacs "24.4") (flycheck "0.22"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; This provides flycheck integration for vale. It allows flycheck to
;; use vale to provide natural language linting.
;;
;; Basic usage:
;;
;;  (require 'flycheck-vale)
;;  (flycheck-vale-setup)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Code:

(require 'flycheck)

(defgroup flycheck-vale nil
  "Variables related to flycheck-vale."
  :prefix "flycheck-vale-"
  :group 'tools)

(defcustom flycheck-vale-modes '(text-mode markdown-mode rst-mode)
  "List of major modes in which to apply this checker."
  :type '(repeat function))

(defvar-local flycheck-vale-enabled t
  "Buffer-local variable determining if flycheck-vale should be applied.")

(defconst flycheck-vale--level-map
  '(("error" . error)
    ("warning" . warning)))

(defun flycheck-vale--output-to-errors (output checker buffer)
  "Parse the full JSON output of vale, OUTPUT, into a sequence of flycheck error structs."
  (let* ((issues (cdar (json-read-from-string output)))
		 (filename (buffer-file-name buffer)))
	(mapcar (lambda (issue)
		 (let-alist issue
		   (flycheck-error-new
			:buffer buffer
			:filename filename
			:checker checker
			:line .Line
			:column (elt .Span 0)
			:message .Message
			:level (assoc-default .Severity flycheck-vale--level-map 'string-equal 'error)
			:id .Check)))
	   issues)))

(flycheck-def-executable-var vale "vale")

(flycheck-define-command-checker 'vale
  "A flycheck checker using vale natural language linting."
  :command '("vale" "--output" "JSON" source)
  :error-parser #'flycheck-vale--output-to-errors
  :predicate (lambda () flycheck-vale-enabled)
  :modes flycheck-vale-modes)

;;;###autoload
(defun flycheck-vale-setup ()
  "Convenience function to setup the vale flycheck checker.

This adds the vale checker to the list of flycheck checkers."
  (add-to-list 'flycheck-checkers 'vale))

;;;###autoload
(defun flycheck-vale-toggle-enabled ()
  "Toggle `flycheck-vale-enabled'."
  (interactive)
  (setq flycheck-vale-enabled (not flycheck-vale-enabled)))

(provide 'flycheck-vale)

;;; flycheck-vale.el ends here

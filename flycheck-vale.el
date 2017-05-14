;;; flycheck-vale.el --- flycheck integration for vale
;; Copyright (c) 2017 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/flycheck-vale
;; Package-Requires: ((emacs "24") (flycheck "0.22") (cl-lib "0.5"))
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

(defcustom flycheck-vale-program "vale"
  "The vale executable to use."
  :type '(string)
  :group 'flycheck-vale)

(defconst flycheck-vale--level-map
  '(("error" . error)
    ("warning" . warning)))

(defun flycheck-vale--issue-to-error (result)
  (let-alist result
    (flycheck-error-new
     :line .Line
     :column (elt .Span 0)
     ;; :buffer (current-buffer)
     ;; :filename "TODO"
     :message .Message
     ;; :checker checker
     :level (assoc-default .Severity flycheck-vale--level-map 'string-equal 'error))))

(defun flycheck-vale--output-to-errors (output)
  (let* ((full-results (json-read-from-string output))
         (result-vecs (mapcar 'cdr full-results))
         (issues (apply 'concatenate 'list (mapcar 'cdr full-results))))
    (mapcar 'flycheck-vale--issue-to-error issues)))

(defun flycheck-vale--start (checker callback)
  "Run vale on the current buffer's contents."

  (let ((orig-buf (current-buffer))
        (outbuf (get-buffer-create "*flycheck-vale-output*")))
    ;; Clear the output buffer
    (with-current-buffer outbuf
      (read-only-mode 0)
      (erase-buffer))

    ;; Run vale
    (call-process-region
     (point-min)
     (point-max)
     flycheck-vale-program
     nil ;; delete
     outbuf
     nil ;; display
     "--output"
     "JSON")

    (with-current-buffer outbuf
      (let ((errors (flycheck-vale--output-to-errors (buffer-string))))
        (loop for err in errors do
              (setf (flycheck-error-buffer err) orig-buf)
              (setf (flycheck-error-filename err) (buffer-file-name orig-buf))
              (setf (flycheck-error-checker err) checker))
        (funcall callback 'finished errors)))))

(defun flycheck-vale--in-supported-mode ()
  "Determines if buffer is in a mode where vale linting is appropriate."
  text-mode)

;;;###autoload
(defun flycheck-vale-setup ()
  "Convenience function to setup the vale flycheck checker.

This adds a hook to watch for ycmd parse results, and it adds the
ycmd checker to the list of flycheck checkers."
  (add-to-list 'flycheck-checkers 'vale))

(flycheck-define-generic-checker 'vale
  "A flycheck checker using vale natural language linting."
  :start #'flycheck-vale--start
  ;; :predicate #'flycheck-vale--in-supported-mode
  :modes '(text-mode))

(provide 'flycheck-vale)

;;; flycheck-vale.el ends here

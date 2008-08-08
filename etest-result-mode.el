;;; etest-result-mode.el --- Watch tests pass or fail

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The default result mode for etest.

(require 'outline)

(declare-function etest-resultp "etest")

;; calm down byte-compiler, you can have this one...
(eval-when-compile
  (defvar current-results)
  (defvar current-meta-info)
  (require 'cl))

;; The grouping of the status is for convenience and use by the
;; font-locking so if you change the re don't forget to capture the
;; status
(defvar etest-rm-not-ok-re "^ *\\(not ok\\) \\.\\."
  "Regexp that will match bad test status.")

(defvar etest-rm-ok-re "^ *\\(ok\\) \\.\\."
  "Regexp that will match good test status.")

(defvar etest-status-re
  (concat "\\(" etest-rm-not-ok-re
          "\\|" etest-rm-ok-re
          "\\)")
  "Regexp that will match a test status.")

(defvar etest-meta-info-re
  (concat "[[:blank:]]"
          (regexp-opt '("total"
                        "pass"
                        "fail"
                        "started"
                        "finished") t)
          "..\\{2,\\}[[:blank:]]+\\(.+\\)$")
  "Regexp that will match the stats at the bottom of the buffer.")

(defvar etest-rm-map
  (let ((m (make-keymap)))
    (define-key m (kbd "q") 'bury-buffer)
    (define-key m (kbd "#") 'etest-rm-cycle-comments)
    (define-key m (kbd "TAB") 'etest-rm-toggle-headline)
    (define-key m (kbd "<tab>") 'etest-rm-toggle-headline)
    m))

(defvar etest-rm-comment-visibility-types
  '(show-all show-not-ok show-ok show-none))

(defvar etest-rm-comment-visibility-map
  '((show-all    etest-rm-show-all-comments)
    (show-not-ok etest-rm-hide-ok-comments)
    (show-ok     etest-rm-hide-not-ok-comments)
    (show-none   etest-rm-hide-all-comments))
  "Defines how the result buffer should look when the user is
toggling visibility states.")

(defgroup etest nil
  "Emacs Testing Framework"
  :group 'lisp)

(defun etest-rm-count-string-at-bol (string)
  "Count how many instances of STRING are at the start of the
current line."
  (save-excursion
    (goto-char (point-at-bol))
    (narrow-to-region (point) (point-at-eol))
    (let ((count 0))
      (while (looking-at string)
        (forward-char)
        (setq count (1+ count)))
      (widen)
      count)))

(defun etest-rm-outline-level ()
  "Calculate what the current outline level should be. See
`ouline-level' for explination."
  ;; we add one becuase there is an extra space before a status
  (1+ (if (looking-at etest-status-re)
          (etest-rm-count-string-at-bol " ")
          (etest-rm-count-string-at-bol "*"))))

;;;###autoload
(defun etest-result-mode (&optional results meta-info)
  "Mode used to display test results."
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (outline-minor-mode)
  (set (make-local-variable 'outline-regexp)
       (concat "\\(\\*\\|"
               etest-status-re "\\|"
               etest-meta-info-re "\\)"))
  (set (make-local-variable 'outline-level)
       'etest-rm-outline-level)
  (set (make-local-variable 'current-results) results)
  (set (make-local-variable 'current-meta-info) meta-info)
  (setq major-mode 'etest-result-mode)
  (setq mode-name "etest-result")
  (set (make-local-variable 'font-lock-defaults)
       '(etest-rm-font-lock-keywords t t))
  (use-local-map etest-rm-map)
  (funcall (cadr (assoc (car etest-rm-comment-visibility-types)
                        etest-rm-comment-visibility-map))))

(defun etest-rm-pretty-print-status (result level)
  "The pretty printing of a single test result. "
  (let ((returned (plist-get result :result)))
    (let* ((doc (plist-get result :doc))
           (comments (plist-get result :comments))
           (prefix (if returned "ok" "not ok")))
      (insert (concat " " prefix " "))
      (insert-char ?\. (- 18 (length prefix) level))
      (insert ".. ")
      (let ((col (current-column)))
      (insert (concat doc "\n"))
      (when comments
        (mapc
         (lambda (comment)
           (indent-to col)
           (insert (concat "# " comment "\n")))
         (split-string comments "\n" t)))))))

(defun etest-rm-pretty-print-results (results &optional level)
  "Pretty print the results of a run to a buffer. See also
`etest-rm-pretty-print-status'."
  (let ((level (or level 0))
        (res (car results)))
    (cond
      ((stringp res)
       (insert-char ?\* (1+ level))
       (insert (concat " " res "\n"))
       (setq level (1+ level)))
      ((etest-resultp res)
       (indent-to level)
       (etest-rm-pretty-print-status res level))
      ((listp res)
       (etest-rm-pretty-print-results res level)))
    (when (cdr results)
      (etest-rm-pretty-print-results (cdr results) level))))

(defun etest-rm-pretty-print-meta-info (meta-info)
  "Insert a few details about the pass rate."
  (let* ((pass (float (plist-get meta-info :pass)))
         (fail (float (plist-get meta-info :fail)))
         (total (+ pass fail))
         (start (plist-get meta-info :timestart))
         (finish (plist-get meta-info :timefinish)))
    (insert (format (concat "\n total ..... %d\n"
                            " pass ...... %-3d (%3d%%)\n"
                            " fail ...... %-3d (%3d%%)")
                    total
                    pass
                    (* (/ pass total) 100)
                    fail
                    (* (/ fail total) 100)))
    (insert (format (concat "\n started ... %s\n"
                            " finished .. %s (%f seconds)\n")
                    (current-time-string start)
                    (current-time-string finish)
                    (- (float-time finish) (float-time start))))))


(defun etest-rm-refresh-buffer (results &optional meta-info)
  "Refresh the results buffer using the cached test results."
  (save-selected-window
    (switch-to-buffer-other-window (get-buffer-create "*etest*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (etest-rm-pretty-print-results results 0)
    (when meta-info
      (etest-rm-pretty-print-meta-info meta-info))
    (etest-result-mode results meta-info)
    (goto-char (point-min))
    (when (search-forward-regexp etest-rm-not-ok-re nil t)
      (goto-char (point-at-bol)))))

(defconst etest-rm-not-ok-face 'etest-rm-not-ok-face)
(defface etest-rm-not-ok-face
    '((default (:inherit font-lock-warning-face)))
  "Face used for failing tests."
  :group 'etest)

(defconst etest-rm-ok-face 'etest-rm-ok-face)
(defface etest-rm-ok-face
    '((default (:inherit font-lock-variable-name-face)))
  "Face used for passing tests."
  :group 'etest)

(defconst etest-rm-comment-face 'etest-rm-comment-face)
(defface etest-rm-comment-face
    '((default (:inherit font-lock-comment-face)))
  "Face used for comments."
  :group 'etes)

(defconst etest-rm-heading-face 'etest-rm-heading-face)
(defface etest-rm-heading-face
    '((default (:inherit font-lock-keyword-face)))
  "Face used for headings."
  :group 'etes)

(defconst etest-rm-font-lock-keywords
  `((,etest-rm-ok-re     1 etest-rm-ok-face)
    (,etest-rm-not-ok-re 1 etest-rm-not-ok-face)
    (,etest-meta-info-re 1 etest-rm-heading-face)
    ("^ *\\(#.+\\)"      1 etest-rm-comment-face)
    ("^ *\\*+ \\(.+\\)"  1 etest-rm-heading-face)))

(defun etest-rm-toggle-headline ()
  "Toggle the visibility of a test category."
  (interactive)
  (unless (looking-at outline-regexp)
    (outline-previous-heading))
  (if (get-char-property (point-at-eol) 'invisible)
      (show-subtree)
      (hide-subtree)))

;;; comment toggling etc...

(defun etest-rm-cycle-comments ()
  "Shift the values in `etest-rm-comment-visibility-types' and
use the `car' of that list to determine the visibility of
comments."
  (interactive)
  (setq etest-rm-comment-visibility-types
        (concatenate 'list
                     (cdr etest-rm-comment-visibility-types)
                     (list (car etest-rm-comment-visibility-types))))
  (etest-rm-refresh-buffer current-results current-meta-info)
  (message (format "%S" (car etest-rm-comment-visibility-types))))

(defmacro etest-with-comments (&rest body)
  "Eval BODY on each comment in the results buffer."
  `(save-excursion
     (goto-char (point-min))
     (while (search-forward-regexp etest-status-re nil t)
       (outline-previous-heading)
       ,@body
       (forward-line))))

(defun etest-rm-hide-not-ok-comments ()
  "Hide all comments associated with a passing test in a result
buffer."
  (interactive)
  (etest-with-comments
   (if (looking-at etest-rm-not-ok-re)
     (hide-subtree)
     (show-subtree))))

(defun etest-rm-hide-ok-comments ()
  "Hide all comments associated with a passing test in a result
buffer."
  (interactive)
  (etest-with-comments
   (if (looking-at etest-rm-ok-re)
     (hide-subtree)
     (show-subtree))))

(defun etest-rm-hide-all-comments ()
  "Hide all comments in a result buffer."
  (interactive)
  (etest-with-comments
   (hide-subtree)))

(defun etest-rm-show-all-comments ()
  "Show all comments in a result buffer."
  (interactive)
  (etest-with-comments
   (show-subtree)))  

(provide 'etest-result-mode)

;;; etest.el --- Run tests and get back a hierarchical set of results.

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

;; etest lets you define tests in a domain specific, hierarchical
;; manner and gather results in a simple, structure of the same shape.

;; To install you must put the location of etest into your
;; `load-path', perhaps like this:
;;
;; (add-to-list 'load-path "~/.elisp/etest")
;;
;; Then actually load etest.el:
;;
;; (require 'etest)
;;
;; Valid examples of etest usage might be:
;;
;; Checking (+ 1 1) yeilds a non-nil result:
;; (etest '(ok (+ 1 1)))
;;
;; You can add an extra argument to the end of any test and it will be
;; used as the documentation string for the test:
;;
;; (etest '(ok (+ 1 1) "Check 1 + 1 yeilds non-nil"))
;;
;; If you omit this string then one will be generated for you.
;;
;; Checking (+ 1 1) yeilds 2:
;; (etest '(eq (+ 1 1) 2))
;;
;; To combine these you might do this:
;; (etest '("Check '+' function" (ok (+ 1 1)) (eq (+ 1 1) 2)))
;;
;; The string is just a header to split things up and hopefully make
;; the output more readable. You can have header groups nest as deeply
;; as you like and within each as many tests as you like.
;;
;; To define your own tests the `deftest' function should be used. For
;; example the following can (and is) used to test etest itself:
;;
;; (deftest '(eres 1)
;;     (lambda (test)
;;       (etest-ok
;;        (plist-get (car (etest-run (list test))) :result))))
;;
;; Used like this:
;;
;; (etest '(eres (ok t)))
;;
;; I can see if etests 'built-ins' are working.

(require 'outline)

(eval-when-compile
  (require 'cl)
  (defvar current-results)
  (defvar current-meta-info))

(defvar etest-results-function 'etest-rm-refresh-buffer
  "Function used to display the results of a run.")

(defvar etest-candidates-plist
  '(eq      (etest-eq 2)
    noerror (etest-noerror 1)
    error   (etest-error 1)
    like    (etest-like 2)
    null    (etest-null 1)
    equal   (etest-equal 2)
    eql     (etest-eql 2)
    ok      (etest-ok 1)
    todo    (etest-todo 1))
  "Plist of test candidates where PROP is the name of the new
test . See `deftest' for details of how to modify this.")

(defun deftest (details func)
  "Define a new test. DETAILS must be a list containing the name
of the test and the argcount. FUNC is the actual function that
will be run."
  (destructuring-bind (name argcount) details
    (plist-put etest-candidates-plist
               name (list func argcount))))

(defun etest-todo (form)
  "Return an etest result set with :result set to t. Set
:todo to t and comments to the result of FORM."
  (let ((res (prin1-to-string
              (condition-case err (car (etest-run (list form)))
              (error
               err)))))
    (list :result t
          :comments (concat "got: " (replace-regexp-in-string "\n" "" res))
          :todo t)))

(defun etest-ok (test)
  "Simply eval TEST and pass if the result is non-nil."
  (let ((ret (eval test))
        (result '()))
    (setq result (plist-put result :result (not (null ret))))
    (setq result (plist-put result :comments (format "got: '%S'" ret)))
    result))

(defun etest-equality-test (func one two)
  "Compare two items, ONE and TWO, using the function
FUNC. Returns a test result."
  (let ((one (eval one))
        (two (eval two))
        (res (funcall func (eval one) (eval two)))
        (result '()))
    (setq result (plist-put result :result res))
    (setq result (plist-put result :comments
                            (if res
                                (format "both: '%S'" one)
                                (format "one: '%S'\ntwo: '%S'" one two))))))

(defun etest-null (test)
  "Allows the use of `null' in a test."
  (let ((ret (eval test))
        (result '()))
    (setq result (plist-put result :result (null ret)))
    (setq result (plist-put result :comments (format "got: '%S'" ret)))
    result))

(defun etest-eq (one two)
  "Allows the use of `eq' in a test."
  (etest-equality-test 'eq one two))

(defun etest-equal (one two)
  "Allows the use of `equal' in a test."
  (etest-equality-test 'equal one two))

(defun etest-eql (one two)
  (etest-equality-test 'eql one two))

(defun etest-noerror (form)
  "Assert FORM evals without error."
  (let ((result (etest-error form)))
    (plist-put result :result (not (plist-get result :result)))))

(defun etest-error (form)
  "Assert FORM evals with error."
  (let* ((result '())
         (val (condition-case err (eval form)
                 (error
                  (setq result (list :result t
                                     :comments (format "got: '%S'" err)))))))
    (if result
        result
        (list :result nil
              :comments (format "got: '%S'" val)))))

(defun etest-resultp (result)
  "Check that RESULT is a vaid test result."
  (and (plist-member result :result)
       (booleanp (plist-get result :result))
       (plist-member result :comments)))

(defun etest-like (form re)
  "Check string is like re"
  (let* ((i 0)
         (match nil)
         (re (eval re))
         (string (eval form))
         (comments (format "   needle: '%s'\n haystack: '%s'\n" re string))
         (res (not (not (string-match re string))))
         (result (list :result res)))
    (while (setq match (match-string (setq i (1+ i)) string))
      (setq comments (concat (or comments "")
                             (format "match %3d: '%s'\n" i match))))
    (plist-put result :comments comments)
    result))

;;;###autoload
(defmacro etest (&rest form)
  "Wrapper to `etest-run'. Will popup a window displaying the
results of the run."
  `(let* ((meta-info (list :pass 0
                           :fail 0
                           :timestart (current-time)
                           :timefinish 0))
          (results (etest-run ',form meta-info)))
     (plist-put meta-info :timefinish (current-time))
     (when (fboundp etest-results-function)
       (funcall etest-results-function results meta-info))
     results))

(defun etest-run (form &optional meta-info)
  "This function does all of the work where actually running the
tests is concerned. Takes a valid etest form and will return a
similarly shaped set of results. "
  (mapcar
   (lambda (test)
     (let ((name (car test)))
       (cond
         ((stringp name)
          (cons name (etest-run (cdr test) meta-info)))
         ((symbolp name)
          (let ((cand (car (plist-get etest-candidates-plist name)))
                (args (cdr test))
                (argcount (cadr (plist-get etest-candidates-plist name)))
                (doc nil))
            (unless cand
              (error "'%s' is not a valid name type" name))
            (if (< (length args) argcount)
                (error "%s needs %d arguments" cand argcount)
                (if (and (eq (length args) (1+ argcount))
                         (stringp (car (last args))))
                    (progn
                      (setq doc (car (last args)))
                      (setq args (delq doc args)))
                    (setq doc (prin1-to-string test))))
            (let ((results (apply cand args)))
              (plist-put results :doc doc)
              (when meta-info
                (etest-meta-info-update-pass-fail results meta-info))
              results))))))
   form))

(defun etest-meta-info-update-pass-fail (result meta-info)
  "Update the pass/fail item in the meta-info plist based on the
resuls in RESULT."
  (let ((type (if (plist-get result :result) :pass :fail)))
    (plist-put meta-info type (1+ (plist-get meta-info type)))))

;; This is defined so that etest can test itself
(defun etest-test-tests (test result)
  "This test is used to test ETest itself. TEST is the test to be
run (in ETest syntax) and RESULT is a plist of items you would
like to compare. See the file etest.etest for example usage."
  (let* ((testres (car (etest-run (list test))))
         (my-res t)
         (res-items '(:result :comments :doc :todo))
         (my-comments (mapconcat
                       (lambda (item)
                         (replace-regexp-in-string "\n" " "
                          (format "%9S %S"
                           item
                           (plist-get testres item))))
                       res-items
                       "\n")))
    (dolist (sym res-items)
      (let ((testval (plist-get testres sym))
            (resultval (plist-get result sym)))
        (when (and (plist-member result sym)
                   (not (equal testval resultval)))
          (setq my-res nil))))
    (list :result my-res :comments my-comments)))

;; Make `etest-test-tests' available
(deftest '(eres 2) 'etest-test-tests)



;; default results mode begins here

;; The grouping of the status is for convenience and used by the
;; font-locking so if you change the re don't forget to capture the
;; status
(defvar etest-rm-not-ok-re "^ *\\(not ok\\) \\.\\."
  "Regexp that will match bad test status.")

(defvar etest-rm-ok-re "^ *\\(ok\\) \\.\\."
  "Regexp that will match good test status.")

(defvar etest-rm-todo-re "^ *\\(todo\\) \\.\\."
  "Regexp that will match good test status.")

(defvar etest-status-re
  (concat "\\(" etest-rm-not-ok-re
          "\\|" etest-rm-ok-re
          "\\|" etest-rm-todo-re
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
           (prefix (if returned
                       (cond
                         ((plist-get result :todo) "todo")
                         (t "ok"))
                       "not ok")))
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
    (,etest-rm-todo-re   1 etest-rm-not-ok-face)
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



;; etest execute begins here

;; This file will aid the execution of a test run. From any buffer
;; simply run `etest-execute' and it will do its best to find a valid
;; etest file and load it.

(make-variable-buffer-local
 (defvar etest-file nil
  "The path of the etest file associated with the current buffer."))

(defvar etest-load-path '("~/.etests")
  "The path of the etest load path.")

(defun etest-execute-get-test-file ()
  "Find a test file by first checking the (buffer local) variable
`etest-file'. Then checking `etest-load-path' for a similarly
named (to the buffer) file. Then looking in `default-directory'."
  (cond
    ((and etest-file (file-exists-p (expand-file-name etest-file)))
     (expand-file-name etest-file))
    ((and buffer-file-name
          (catch 'found
            (let ((etest-load-path (append etest-load-path
                                           (list default-directory)
                                           load-path))
                  (name (concat
                         (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name)) ".etest")))
              (mapc (lambda (d)
                      (let ((name (expand-file-name (concat d "/" name))))
                        (when (file-exists-p name)
                          (throw 'found name))))
                    etest-load-path)
              nil))))))

(defun etest-execute ()
  "Execute a run for the current file using
`etest-execute-get-test-file'."
  (interactive)
  (let ((file (etest-execute-get-test-file)))
    (unless file
      (error "No matching .etest file found"))
    (load-file file)))

(provide 'etest)

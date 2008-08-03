;;; etest.el --- Run tests and get back a hierarchical set of results.

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.1

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

(require 'etest-result-mode)

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
    ok      (etest-ok 1))
  "Plist of test candidates where PROP is the name of the new
test . See `deftest' for details of how to modify this.")

(defun deftest (details func)
  "Define a new test. DETAILS must be a list containing the name
of the test and the argcount. FUNC is the actual function that
will be run."
  (let ((name (car details))
        (argcount (cadr details)))
    (plist-put etest-candidates-plist
               name (list func argcount))))

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
  (etest-ok `(null ,test)))

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
         (comments (concat "searching: '" string "'\n"))
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
  `(let ((results (etest-run ',form)))
     (when (fboundp etest-results-function)
       (funcall etest-results-function results))
     results))

(defun etest-run (form)
  "This function does all of the work where actually running the
tests is concerned. Takes a valid etest form and will return a
similarly shaped set of results. "
  (mapcar
   '(lambda (test)
     (let ((name (car test)))
       (cond
         ((stringp name)
          (cons name (etest-run (cdr test))))
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
            (plist-put (apply cand args) :doc doc))))))
   form))

;; This is defined so that etest can test itself
(defun etest-test-tests (test result)
  "This test is used to test ETest itself. TEST is the test to be
run (in ETest syntax) and RESULT is a plist of items you would
like to compare. See the file etest.etest for example usage."
  (let ((testres (car (etest-run (list test))))
        (my-res t)
        (my-comments ""))
    (when (null (plist-get testres :result))
      (setq my-comments "note: test result was nil\n"))
    (dolist (sym '(:result :comments :doc))
      (let ((testval (plist-get testres sym))
            (resultval (plist-get result sym)))
        (when (and (plist-member result sym)
                   (not (equal testval resultval)))
          (setq my-res nil)
          (setq my-comments
                (concat my-comments
                        (format "got: %S from '%S'\n"
                                resultval sym))))))
    (list :result my-res :comments my-comments)))

;; Make `etest-test-tests' available
(deftest '(eres 2) 'etest-test-tests)

(provide 'etest)

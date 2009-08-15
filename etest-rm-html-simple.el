(require 'xmlgen)

(setq etest-results-function
      'etest-rm-html-simple)

(defvar etest-rm-html-stylesheet-name "file:///home/phil/style.css"
  "filename of the linked stylesheet to use")

(defvar etest-rm-html-output-dir "~/tmp"
  "test results go here")

(defvar etest-rm-html-output-timestring "%Y-%m-%d-%H-%M-%S"
  "The format string which is resolved with `format-time-string'
and put into the filename of a test run.")

(defvar etest-rm-html-output-extension ".html"
  "The filename extension for the filename of a test run")

(defun etest-rm-html-simple (results &optional meta-info)
  (set-buffer (get-buffer-create "*etest-simple-html-output*"))
  (erase-buffer)
  (insert
   (xmlgen
    `(html
      (head
       (link :rel "stylesheet"
             :href ,etest-rm-html-stylesheet-name
             :type "text/css")
       (meta :http-equiv "Content-Type"
             :content "text/html; charset=UTF-8"))
      (body
       ,(etest-rm-html-meta-info meta-info)
       ,(etest-rm-html-heirarchy results 1)))))
  (write-file (concat etest-rm-html-output-dir
                      "/"
                      (format-time-string etest-rm-html-output-timestring)
                      etest-rm-html-output-extension)))

(defun etest-rm-html-meta-info (meta-info)
  (let* ((details
          `(("total passed" . ,(number-to-string (plist-get meta-info :pass)))
            ("total failed" . ,(number-to-string (plist-get meta-info :fail)))
            ("time started" . ,(current-time-string
                                (plist-get meta-info :timestart)))
            ("time finished" . ,(current-time-string
                                 (plist-get meta-info :timefinish))))))
    (mapconcat
     (lambda (item)
       (xmlgen
        `(div :class "meta"
              (span :class "key" ,(car item))
              ": "
              (span :class "value" ,(cdr item)))))
     details
     "\n")))

(defun etest-rm-html-result (result)
  "The pretty printing of a single test result. "
  (let* ((returned (xmlgen-string-escape (plist-get result :result)))
         (doc (xmlgen-string-escape (plist-get result :doc)))
         (comments (xmlgen-string-escape (plist-get result :comments)))
         (class (if returned "pass" "fail")))
    (xmlgen
     `(div :class ,class
           (div :class "result"
                (div :class "doc" ,doc)
                (div :class "comments"
                     (code ,comments)))))))

(defun etest-rm-html-heirarchy (results &optional level)
  (let ((level (or level 0)))
    (mapconcat
     (lambda (r)
       (cond
        ((stringp r)
         (setq heading (if (< level 5) (number-to-string level) "5"))
         (xmlgen (list (intern (concat "h" heading)) r)))
        ((etest-resultp r)
         (etest-rm-html-result r))
        ((listp r)
         (xmlgen `(div :class ,(concat "level-" (number-to-string level))
                       ,(etest-rm-html-heirarchy r (1+ level)))))
        (t
         "\n")))
     results
     "\n")))

(provide 'etest-rm-html-simple)

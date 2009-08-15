(require 'xmlgen)

(setq etest-results-function
      'etest-rm-html-simple)

(defun etest-rm-html-simple (results &optional meta-info)
  (set-buffer (get-buffer-create "*etest-simple-html-output*"))
  (erase-buffer)
  (html-mode)
  (insert
   (xmlgen
    `(html
      (head
       (link :rel "stylesheet"
             :href "style.css"
             :type "text/css")
       (meta :http-equiv "Content-Type"
             :content "text/html; charset=UTF-8"))
      (body
       ,(etest-rm-html-heirarchy results 1))))))

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
          ;; yuck, all this to make a h1 .. h5 :/
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

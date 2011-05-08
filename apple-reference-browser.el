(eval-when-compile (require 'cl))
(require 'json)
(require 'anything)
(require 'w3m-load)

(defvar arb:candidates nil)

(defvar arb:cache-name-prefix ".arb-candidates-")

(defcustom arb:cache-directory
  "~/"
  "anything candidates cache file directory")

(defcustom arb:docset-path
  "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiOS4_2.iOSLibrary.docset"
  "docset path")

(defcustom arb:open-w3m-other-buffer t "w3m open in other buffer")

(defun arb:docset-lib-json-path ()
  (format "%s%s" 
          arb:docset-path
          "/Contents/Resources/Documents/navigation/library.json"))

(defun arb:read-json (path)
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (search-forward "''" nil t) (replace-match "\"\""))
        (goto-char (point-min))
        (json-read))))

(defun arb:remove-anchor (url)
  (car (split-string url "#")))

(defun arb:remove-last-path-component (url)
  (let* ((strs (split-string url "/"))
         (len (length strs)))
    (setcdr (nthcdr (- len 2) strs) nil)
    (mapconcat 'identity strs "/")))

(defun arb:book-json-path (doc-path)
  (let* ((json-dir (arb:remove-last-path-component
                    (arb:remove-last-path-component doc-path)))
         (json-path (format "%s/book.json" json-dir)))
    (cond ((file-exists-p json-path) json-path)
          (t
           (format "%s/book.json" 
                   (arb:remove-last-path-component doc-path))))))

(defun arb:absolute-doc-path (doc-path)
  (format "%s/%s" 
          (arb:remove-last-path-component (arb:docset-lib-json-path))
          doc-path))

(defvar arb:api-title-list
  '("Class Methods" "Instance Methods" "Functions" "Notifications" "Constants" "Properties"))

(defun arb:abbrev-title (title)
  (cond ((string= title "Class Methods") "Clsm")
        ((string= title "Instance Methods") "Insm")
        ((string= title "Properties") "Prop")
        ((string= title "Constants") "Cnst")
        ((string= title "Notifications") "Notf")
        ((string= title "Functions") "Func")))

(defun arb:get-apis (doc)
  (let* ((book-path (arb:book-json-path (arb:absolute-doc-path 
                                        (arb:remove-anchor (elt doc url-index)))))
         (book (arb:read-json book-path))
         (sym-sections (remove-if-not (lambda (x) (member (cdr (assq 'title x)) arb:api-title-list))
                                      (append (cdr (assq 'sections book)) nil)))
         (book-name (assq 'title book))
         ret)
    (dolist (section (append sym-sections nil))
      (let ((apis (append (cdr (assq 'sections section)) nil))
            (section-title (cdr (assq 'title section))))
        (dolist (api apis)
          (push (cons (format "%s;[%s] %s"
                              (car (split-string (cdr book-name) " "))
                              (arb:abbrev-title section-title)
                              (cdr (assq 'title api)))
                      (arb:file-url
                       (format "%s/%s"
                               (arb:remove-last-path-component book-path)
                               (cdr (assq 'href api)))))
                ret))))
    ret))

(defun arb:file-url (file-path)
  (format "file://%s" file-path))

(defun arb:init (&optional remove-cache)
  (when (or remove-cache (not (file-exists-p (arb:class-cache-path))))
    (let* ((lib (arb:read-json (arb:docset-lib-json-path)))
           (cols (cdr (assq 'columns lib)))
           (url-index (cdr (assq 'url cols)))
           (name-index (cdr (assq 'name cols)))
           (type-index (cdr (assq 'type cols)))
           (reference-num 10)
           (docs (remove-if-not
                  (lambda (x) (equalp (elt x type-index) reference-num))
                  (cdr (assq 'documents lib))))
           (classes (remove-if-not
                     (lambda (x) (string-match "Class" (elt x 0)))
                     docs))
           (protocols (remove-if-not
                     (lambda (x) (string-match "Protocol" (elt x 0)))
                     docs))
           (additions (remove-if-not
                     (lambda (x) (string-match "Additions" (elt x 0)))
                     docs))
           (others (remove-if
                     (lambda (x) (string-match "Class\\|Protocol\\|Additions" (elt x 0)))
                     docs)))
      (arb:write-file-cache 
       (apply 'append (mapcar (lambda (x)
                                (arb:get-apis x))
                              classes))
       (arb:class-cache-path))
      (arb:write-file-cache 
       (apply 'append (mapcar (lambda (x)
                                (arb:get-apis x))
                              protocols))
       (arb:protocol-cache-path))
      (arb:write-file-cache 
       (apply 'append (mapcar (lambda (x)
                                (arb:get-apis x))
                              additions))
       (arb:additions-cache-path))
      (arb:write-file-cache 
       (apply 'append (mapcar (lambda (x)
                                (arb:get-apis x))
                              others))
       (arb:other-cache-path)))))

(defun arb:write-file-cache (apis file-name)
  (with-temp-file (concat "~/" file-name)
    (dolist (api apis)
      (insert (car api))
      (insert ",")
      (insert (cdr api))
      (insert "\n"))))

(defun arb:open-w3m (url)
  (cond
   (arb:open-w3m-other-buffer
    (let* ((urls (split-string url "#"))
           (url (car urls))
           (anchor (cadr urls)))
      (save-window-excursion 
        (w3m-browse-url url nil) 
        (get-buffer "*w3m*"))
      (save-selected-window 
        (pop-to-buffer "*w3m*" t)
        (if anchor (w3m-search-name-anchor anchor)))))
   (t
    (w3m-browse-url url nil))))

(defun arb:unescape-name (name)
  (replace-regexp-in-string "&quot;" "\""
   (replace-regexp-in-string "&amp;" "&"
    (replace-regexp-in-string "&lt;" "<"
     (replace-regexp-in-string "&gt;" ">" name)))))

(defun arb:insert (url)
  (let ((path (car (split-string (substring url 7) "#"))))
    (with-temp-buffer
      (insert-file-contents path)
      (re-search-backward "<div class=\"declaration\">(.*)</div>")
      )
    (insert (match 0))))

(defmacro arb:defsource (symbol-name name file)
  (let ((f (eval file)))
    `(defvar ,symbol-name
       '((name . ,name)
         (candidates-file . ,f)
         (filtered-candidate-transformer 
          . (lambda (cands source)
              (mapcar (lambda (x) 
                        (let ((s (split-string x ",")))
                          (cons (car s) (cadr s))))
                      cands)))
         (requires-pattern . 2)
         (action . (("w3m" . arb:open-w3m)
                    ("Default Browser" . browse-url)
                    ("insert" . arb:insert)))))))

(defun arb:class-cache-path ()
  ;; (concat arb:cache-directory arb:cache-name-prefix "class"))
  (concat arb:cache-name-prefix "class"))

(defun arb:protocol-cache-path ()
  ;; (concat arb:cache-directory arb:cache-name-prefix "protocol"))
  (concat arb:cache-name-prefix "protocol"))

(defun arb:additions-cache-path ()
  ;; (concat arb:cache-directory arb:cache-name-prefix "additions"))
  (concat arb:cache-name-prefix "additions"))

(defun arb:other-cache-path ()
  ;; (concat arb:cache-directory arb:cache-name-prefix "other"))
  (concat arb:cache-name-prefix "other"))

(arb:defsource anything-c-source-apple-reference-class
               "Class" 
               (arb:class-cache-path))
(arb:defsource anything-c-source-apple-reference-protocol
               "Protocol" 
               (arb:protocol-cache-path))
(arb:defsource anything-c-source-apple-reference-additions
               "Additions" 
               (arb:additions-cache-path))
(arb:defsource anything-c-source-apple-reference-other
               "Other"
               (arb:other-cache-path))

(defun arb:search ()
  (interactive)
  (arb:init)
  (let ((anything-display-function
         (lambda (buf)
           (if anything-samewindow 
               (funcall 'switch-to-buffer buf)
             (funcall 'pop-to-buffer buf t)))))
    (anything-other-buffer '(anything-c-source-apple-reference-class
                             anything-c-source-apple-reference-protocol
                             anything-c-source-apple-reference-additions
                             anything-c-source-apple-reference-other)
                           "reference library")))


;(arb:init t)
;(setq arb:open-w3m-other-buffer t)
;(arb:search)

(provide 'apple-reference-browser)
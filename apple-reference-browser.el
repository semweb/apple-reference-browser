(eval-when-compile (require 'cl))
(require 'json)
(require 'anything)
(require 'w3m-load)

(defvar arb:candidates nil)

(defvar arb:cache-name "apple-reference-browser-candidates.el")

(defcustom arb:cache-path
  (format "~/.emacs.c/lisp/%s" arb:cache-name)
  "anything candidates cache file path")

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

(defun arb:get-apis (book book-path)
  (let ((sym-sections (remove-if-not (lambda (x) (member (cdr (assq 'title x)) arb:api-title-list))
                                     (append (cdr (assq 'sections book)) nil)))
        (book-name (assq 'title book))
        ret)
    (dolist (section (append sym-sections nil))
      (let ((apis (append (cdr (assq 'sections section)) nil))
            (section-title (cdr (assq 'title section))))
        (dolist (api apis)
          (push (cons (format "[%s][%s] %s"
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
  (if (and (file-exists-p arb:cache-path) (not remove-cache))
      (load arb:cache-path)
    (let* ((lib (arb:read-json (arb:docset-lib-json-path)))
           (cols (cdr (assq 'columns lib)))
           (url-index (cdr (assq 'url cols)))
           (name-index (cdr (assq 'name cols)))
           (type-index (cdr (assq 'type cols)))
           (reference-num 10)
           (docs (remove-if-not
                  (lambda (x) (equalp (elt x type-index) reference-num))
                  (cdr (assq 'documents lib))))
           candidates)
      (dolist (doc (append docs nil))
        (let* ((book-url (arb:book-json-path (arb:absolute-doc-path 
                                              (arb:remove-anchor (elt doc url-index)))))
               (book (arb:read-json book-url))
               (apis (arb:get-apis book book-url)))
          (setq candidates (append candidates apis))))
      (setq arb:candidates (nreverse candidates))
      (arb:save-cache))))

(defun arb:save-cache ()
  (with-temp-buffer
    (insert "(setq arb:candidates '")
    (insert (prin1-to-string arb:candidates))
    (insert ")")
    (write-file arb:cache-path)))

(defun arb:open-w3m (url)
  (cond
   (arb:open-w3m-other-buffer
    (let ((b (save-window-excursion (w3m-browse-url url nil) (get-buffer "*w3m*"))))
      (ignore-errors (save-selected-window (pop-to-buffer "*w3m*")))))
   (t
    (w3m-browse-url url nil))))

(defun arb:unescape-name (name)
  (replace-regexp-in-string "&quot;" "\""
   (replace-regexp-in-string "&amp;" "&"
    (replace-regexp-in-string "&lt;" "<"
     (replace-regexp-in-string "&gt;" ">" name)))))

(defun arb:search ()
  (interactive)
  (anything 'anything-c-source-apple-reference))

(defvar anything-c-source-apple-reference
  '((name . "Reference Library")
    (candidates . arb:candidates)
    (action . (("w3m" . arb:open-w3m)
               ("Default Browser" . browse-url)))))

;(arb:init t)
;(setq arb:open-w3m-other-buffer t)
;(arb:search)

(provide 'apple-reference-browser)

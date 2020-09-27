;; org-roam-tag-management.el
(require 'org-roam-db)

(defcustom org-roam-tag-data-types '(
                                  (fuzzy . ((format . "%%%s%%")
                                            (replace . (lambda (keyword before after)
                                                        (replace-regexp-in-string before after keyword)
                                                        ))
                                            ))
                                  (exact . ((format . "%%\"%s\"%%")
                                            (replace . (lambda (keyword before after)
                                                        (if (string=  before keyword)
                                                            (replace-regexp-in-string before after keyword)
                                                          keyword)
                                                        ))
                                            ))
                                  )
  "search type. fuzzy or exact or any other function.")

(defcustom org-roam-tag-data-type 'fuzzy
   "search type"         )

(defun org-roam-get-all-tags()
  (delete-dups (-flatten (org-roam-db-query
                          [:select [tags]
                           :from tags] ))))

(defun org-roam-number-of-files-with-tag (tag)
  (length (org-roam-files-with-tag tag)))

(defun org-roam--get-format-string ()
   (condition-case err
       (cdr (assoc 'format (cdr (assoc org-roam-tag-data-type org-roam-tag-data-types))))
   (error (message "make sure format is defined in %s in org-roam-tag-data-types.\n%s" org-roam-tag-data-type err) (signal (car err) (cdr err))
  ))
  )

(defun org-roam--get-replace-function ()
  (condition-case err
      (cdr (assoc 'replace (cdr (assoc org-roam-tag-data-type org-roam-tag-data-types))))
    (error (message "make sure replace is defined in %s in org-roam-tag-data-types.\n%s" org-roam-tag-data-type err) (signal (car err) (cdr err))
           ))
  )

(defun org-roam-files-with-tag (tag)
  (org-roam-files-with-type-tag tag (org-roam--get-format-string))
  )

(defun org-roam-files-with-type-tag (tag f)
  (org-roam-db-query
   [:select [file]
    :from tags
    :where (like tags  $r1)] (format f tag)))


(defun org-roam--buffer-rename-tag (keywords before after)
  "takes a array of string and replaces a value that matches."
  (mapcar #'(lambda (x)
             ( let (
                    ( val (funcall (org-roam--get-replace-function) x before after))
                    )
               (if (string-match-p (regexp-quote " ") val)
                   (format "\"%s\"" val)
                 val) ;can leave out the quotes if not needed
               ))
          keywords))

(defun org-roam--buffer-find-and-rename-tag (before after)
  (let (keywords after-list concat-string)
    (setq keywords (split-string-and-unquote (car (cdr (car (org-collect-keywords '("roam_tags")))))))
    (setq after-list (org-roam--buffer-rename-tag keywords before after))
    (setq concat-string (mapconcat 'identity after-list " "))
    (format "#+roam_tags: %s" concat-string)
    ))

(defun org-roam--open-file-rename-tag-and-close-file (file before after)
  (org-roam-with--file file
  (let (to-write)
    (find-file file)
    (setq to-write (org-roam--buffer-find-and-rename-tag before after))
    (goto-char (point-min))
    (re-search-forward "^#\\+roam_tags:.*" nil t)
    (replace-match to-write)
    (save-buffer)
    )))

(defun org-roam-rename-tag (before after)
  (let ((files-to-rename-tag (-flatten (org-roam-files-with-tag before))))
    (save-window-excursion
    (save-excursion
      (save-restriction
        (mapcar #'(lambda (file)
                   (org-roam--open-file-rename-tag-and-close-file file before after)
                   )
                files-to-rename-tag)))))
  )

(provide 'org-roam-tag-management)

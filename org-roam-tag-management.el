(setq-local db (emacsql-sqlite3 (expand-file-name "org-roam.db" org-roam-directory)))
(defun my/org-roam-number-of-files-with-tag(tag)
  (emacsql  db
            [:select [(funcall count file)]
                     :from tags
                     :where (like tags  $r1)] (format "%%%s%%" tag)))

(setq-local db (emacsql-sqlite3 (expand-file-name "org-roam.db" org-roam-directory)))
(defun my/org-roam-files-with-tag(tag)
  (emacsql  db
            [:select [file]
                     :from tags
                     :where (like tags  $r1)] (format "%%%s%%" tag)))

(defun my/org-roam-file-replace-tag (keywords before after)
     ;takes a array of string and replaces a value that matches.
      (mapcar '(lambda (x)
                 ( let (
                        ( my/val (if (string=  before x)
                                     (replace-regexp-in-string before after x)
                                   x))
                        )
                   (if (string-match-p (regexp-quote " ") my/val)
                       (format "\"%s\"" my/val)
                     my/val) ;can leave out the quotes if not needed
                 ))
              keywords))

  (defun my/org-roam-replace-text (before after)
    (setq-local my/keywords (split-string-and-unquote (car (cdr (car (org-collect-keywords '("roam_tags")))))))
    (setq-local my/after-list (my/org-roam-file-replace-tag my/keywords before after))
    (setq-local my/concat-string (mapconcat 'identity my/after-list " "))
    (format "#+roam_tags: %s" my/concat-string)
    )


(defun my/org-roam-file-replace-tag (keywords before after)
           ;takes a array of string and replaces a value that matches.
            (mapcar '(lambda (x)
                       ( let (
                              ( my/val (if (string=  before x)
                                           (replace-regexp-in-string before after x)
                                         x))
                              )
                         (if (string-match-p (regexp-quote " ") my/val)
                             (format "\"%s\"" my/val)
                           my/val) ;can leave out the quotes if not needed
                       ))
                    keywords))

        (defun my/org-roam-replace-text (before after)
          (setq-local my/keywords (split-string-and-unquote (car (cdr (car (org-collect-keywords '("roam_tags")))))))
          (setq-local my/after-list (my/org-roam-file-replace-tag my/keywords before after))
          (setq-local my/concat-string (mapconcat 'identity my/after-list " "))
          (format "#+roam_tags: %s" my/concat-string)
          )

    ;; to-do
        (defun my/open-and-write-file (file before after)
          (find-file file)
          (setq-local my/to-write (my/org-roam-replace-text before after))
          (goto-char (point-min))
          (re-search-forward "^#\\+roam_tags:.*" nil t)
          (replace-match my/to-write)
          (save-buffer)
          (kill-buffer)
          )

        (defun my/org-roam-rename-tag-of-file (before after)
          (setq-local files-to-replace (-flatten (my/org-roam-files-with-tag before)))
          (save-excursion
           (save-restriction
          (mapcar '(lambda (file)
                     (my/open-and-write-file file before after)
                     )
                  files-to-replace))))


(setq-local db (emacsql-sqlite3 (expand-file-name "org-roam.db" org-roam-directory)))
(defun my/org-roam-tags()
  (delete-dups (-flatten (emacsql  db
            [:select [tags]
                     :from tags] ))))

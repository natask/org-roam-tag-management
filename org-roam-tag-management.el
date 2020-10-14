;;; org-roam-tag-management.el --- Manage Tags of Org Roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Natnael Kahssay <thisnkk@gmail.com>

;; Author: Natnael Kahssay <thisnkk@gmail.com>
;; URL: https://github.com/natask/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.2
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides the org-roam-tag-management functionality for org-roam
;;; Code:
;;;; Library Requires
(require 'org-roam-db)

(defmacro org-roam--with-file (file &rest body)
  "Execute BODY within a FILE.
Closes the file if the file is not yet visited."
  (declare (indent 1) (debug t))
  `(let* ((existing-buf (find-buffer-visiting ,file))
          (res ,@body))
     ;; find-buffer-visiting needs to be recomputed because it was created by
     ;; @body
     (unless existing-buf (kill-buffer (find-buffer-visiting ,file)))
     res))

;;; variables
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
  "Types on how to treat tag search. fuzzy or exact or any other custom type."
  :type '(alist (group (alist (group sexp)))) 
  :require 'org-roam-mode
  :group 'org-roam
  )

(defcustom org-roam-tag-data-type 'fuzzy
  "Default tag search type."
  :type 'symbol
  :require 'org-roam-mode
  :group 'org-roam
)

;;; Variable access functions
(defun org-roam--get-format-string ()
  "Get the format string of current tag data type."
   (condition-case err
       (cdr (assoc 'format (cdr (assoc org-roam-tag-data-type org-roam-tag-data-types))))
   (error (message "make sure format is defined in %s in org-roam-tag-data-types.\n%s" org-roam-tag-data-type err) (signal (car err) (cdr err))
  ))
  )

(defun org-roam--get-replace-function ()
  "Get the replace function of current tag data type."
  (condition-case err
      (cdr (assoc 'replace (cdr (assoc org-roam-tag-data-type org-roam-tag-data-types))))
    (error (message "make sure replace is defined in %s in org-roam-tag-data-types.\n%s" org-roam-tag-data-type err) (signal (car err) (cdr err))
           ))
  )

;;; Tag functions
;;;; Tag all functions
(defun org-roam-get-all-tags()
  "Get all org roam tags."
  (delete-dups (-flatten (org-roam-db-query
                          [:select [tags]
                           :from tags] ))))

;;;; Tag file functions
(defun org-roam-files-with-type-tag (tag format-string)
  "List files that match `tag' under search using `format-string'."
  (org-roam-db-query
   [:select [file]
    :from tags
    :where (like tags  $r1)] (format format-string tag)))


(defun org-roam-files-with-tag (tag)
  "List files that match `tag' under search using current `org-roam-tag-data-type'."
  (org-roam-files-with-type-tag tag (org-roam--get-format-string))
  )

(defun org-roam-number-of-files-with-tag (tag)
  "Find number of tiles with `tag'."
  (length (org-roam-files-with-tag tag)))

;;;; Tag rename functions
(defun org-roam--buffer-rename-tag (keywords before after)
  "Takes a array of strings, `keywords' and replaces string matching`before' with `after'."
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
  "Rename tag from `before' to `after' on current buffer.
   Currently only supports tags defined under `roam-tags' property."
  (let (keywords after-list concat-string)
    (setq keywords (split-string-and-unquote (car (cdr (car (org-collect-keywords '("roam_tags")))))))
    (setq after-list (org-roam--buffer-rename-tag keywords before after))
    (setq concat-string (mapconcat 'identity after-list " "))
    (format "#+roam_tags: %s" concat-string)
    ))

(defun org-roam--open-file-rename-tag-and-close-file (file before after)
  "Rename tag from `before' to `after' on file."
  (org-roam--with-file file
  (let (to-write)
    (with-current-buffer (find-file-noselect file)
    (setq to-write (org-roam--buffer-find-and-rename-tag before after))
    (goto-char (point-min))
    (re-search-forward "^#\\+roam_tags:.*" nil t)
    (replace-match to-write)
    (save-buffer)
    ))))

(defun org-roam-rename-tag (before after)
  "Rename tag from `before' to `after' on all org roam files.
   Currently only supports tags defined under `roam-tags' property."
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

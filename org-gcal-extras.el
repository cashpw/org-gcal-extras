;;; org-gcal-extras.el --- Extras for org-gcal -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: December 17, 2024
;; Modified: December 17, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashweaver/org-gcal-extras
;; Package-Requires: ((emacs "24.3") (org-gcal "0.4.2") (cl-lib "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct org-gcal-profile
  "A profile for `org-gcal'."
  (fetch-file-alist
   nil
   :type 'sexp
   :documentation "Sets `org-gcal-fetch-file-alist'.")
  (client-id nil :type 'string :documentation "Sets `org-gcal-client-id'.")
  (client-secret
   nil
   :type 'string
   :documentation "Sets `org-gcal-client-secret'.")
  (after-update-entry-functions
   nil
   :type 'hook
   :documentation "Sets `org-gcal-after-update-entry-functions'.")
  (fetch-event-filters
   nil
   :type 'hook
   :documentation "Sets `org-gcal-fetch-event-filters'.")
  (summaries-to-skip
   nil
   :type '(repeat string)
   :documentation "Sets `org-gcal-extras--summaries-to-skip'.")
  (categories
   nil
   :type 'sexp
   :documentation "Sets `org-gcal-extras--categories'.")
  (tags nil :type 'sexp :documentation "Sets `org-gcal-extras--tags'.")
  (on-activate nil :type 'sexp :documentation "Run on profile activation."))

(defcustom org-gcal--current-profile nil
  "The current active profile, set in `org-gcal-activate-profile'."
  :type 'org-gcal-profile
  :group 'org-gcal)

(defcustom org-gcal-extras-processed-tag "processed"
  "Tag which is set on a heading to indicate we've processed it already."
  :type 'string
  :group 'org-gcal)

(defvar org-gcal-extras--categories '()
  "Alist of (subject . category).

See `org-gcal-extras--set-category.'")

(defvar org-gcal-extras--tags '()
  "Alist of (subject . tags).

Tags can be in any of the following formats:

- String representing a single tag; for example: \"foo\"
- String representing a list of tags; for example: \":foo:bar:\"
- List of strings; for example: (\"foo\" \"bar\")

See `org-gcal-extras--set-tags.'")

(defvar org-gcal-extras--summaries-to-skip '()
  "List of event summaries to filter out.

See `org-gcal-extras--skip-event-by-summary-p'")

(cl-defun org-gcal-activate-profile (profile)
  "Set appropriate `org-gcal' variables based on PROFILE."
  (setq
   org-gcal--current-profile profile
   org-gcal-after-update-entry-functions nil

   org-gcal-client-id (org-gcal-profile-client-id profile)
   org-gcal-client-secret (org-gcal-profile-client-secret profile)
   org-gcal-fetch-file-alist (org-gcal-profile-fetch-file-alist profile)

   org-gcal-extras--summaries-to-skip (org-gcal-profile-summaries-to-skip profile)
   org-gcal-extras--categories (org-gcal-profile-categories profile)
   org-gcal-extras--tags (org-gcal-profile-tags profile))

  (funcall (org-gcal-profile-on-activate profile))

  (add-hook
   'org-gcal-after-update-entry-functions 'org-gcal-extras--set-processed)

  (setq org-gcal-fetch-event-filters nil)
  (add-hook
   'org-gcal-fetch-event-filters
   'org-gcal-extras--skip-event-by-summary-p)
  (dolist (fn (reverse (org-gcal-profile-fetch-event-filters profile)))
    (add-hook 'org-gcal-fetch-event-filters fn))

  (dolist (fn (reverse (org-gcal-profile-after-update-entry-functions profile)))
    (add-hook 'org-gcal-after-update-entry-functions fn))

  (when (fboundp 'org-gcal-reload-client-id-secret)
    (org-gcal-reload-client-id-secret)))

(defun org-gcal-extras--add-tag (tag)
  "Add TAG to `org-mode' heading at point."
  (org-set-tags
   (cl-remove-duplicates (cons tag (org-get-tags nil 'local)) :test #'string=)))

(defun org-gcal-extras--set-processed (_calendar-id _event _update-mode)
  "Set the processed tag on the heading at point."
  (save-excursion (org-gcal-extras--add-tag org-gcal-extras-processed-tag)))

(defun org-gcal-extras--processed-p ()
  "Return non-nil if heading at point has been processed."
  (member org-gcal-extras-processed-tag (org-get-tags (point) t)))

(defun org-gcal-extras--remove-gcal-timestamp ()
  "Delete the timestamp inserted by `org-gcal'."
  (save-excursion
    (org-mark-subtree)
    (replace-regexp org-element--timestamp-regexp ""
                    nil
                    (region-beginning)
                    (region-end))
    (deactivate-mark)))

(defun org-gcal-extras--timestamp (start end)
  "Return active `org-mode' timestamp string from START to END."
  (cond
   ((or (string= start end) (org-gcal--alldayp start end))
    (org-gcal--format-iso2org start))
   ((and (= (plist-get (org-gcal--parse-date start) :year)
            (plist-get (org-gcal--parse-date end) :year))
         (= (plist-get (org-gcal--parse-date start) :mon)
            (plist-get (org-gcal--parse-date end) :mon))
         (= (plist-get (org-gcal--parse-date start) :day)
            (plist-get (org-gcal--parse-date end) :day)))
    (format "<%s-%s>"
            (org-gcal--format-date start "%Y-%m-%d %a %H:%M")
            (org-gcal--format-date end "%H:%M")))
   (t
    (format "%s--%s"
            (org-gcal--format-iso2org start)
            (org-gcal--format-iso2org
             (if (< 11 (length end))
                 end
               (org-gcal--iso-previous-day end)))))))

(defun org-gcal-extras--timestamp-from-event (event)
  "Return `org-mode' timestamp for EVENT duration."
  (let* ((start-time (plist-get (plist-get event :start) :dateTime))
         (end-time (plist-get (plist-get event :end) :dateTime))
         (start-day (plist-get (plist-get event :start) :date))
         (end-day (plist-get (plist-get event :end) :date))
         (start
          (if start-time
              (org-gcal--convert-time-to-local-timezone
               start-time org-gcal-local-timezone)
            start-day))
         (end
          (if end-time
              (org-gcal--convert-time-to-local-timezone
               end-time org-gcal-local-timezone)
            end-day))
         (old-time-desc (org-gcal--get-time-and-desc))
         (old-start (plist-get old-time-desc :start))
         (old-end (plist-get old-time-desc :start))
         (recurrence (plist-get event :recurrence)))
    ;; Keep existing timestamps for parent recurring events.
    (when (and recurrence old-start old-end)
      (setq
       start old-start
       end old-end))
    (org-gcal-extras--timestamp start end)))

(defun org-gcal-extras--set-scheduled (_calendar-id event _update-mode)
  "Set scheduled property for EVENT at point.

See `org-gcal-after-update-entry-functions'."
  (save-excursion
    (unless (org-gcal-extras--processed-p)
      (org-gcal-extras--remove-gcal-timestamp)
      (org-schedule nil (org-gcal-extras--timestamp-from-event event)))))

(defun org-gcal-extras--set-category (_calendar-id event _update-mode)
  "Set appropriate category for EVENT."
  (save-excursion
    (unless (org-gcal-extras--processed-p)
      (when-let ((summary (plist-get event :summary))
                 (category (alist-get summary org-gcal-extras--categories)))
        (org-set-property "CATEGORY" category)))))

(defun org-gcal-extras--set-tags (_calendar-id event _update-mode)
  "Set appropriate tag for EVENT."
  (save-excursion
    (unless (org-gcal-extras--processed-p)
      (when-let ((summary (plist-get event :summary))
                 (tags (alist-get summary org-gcal-extras--tags)))
        (cond
         ((string-match-p ":" tags)
          (mapcar
           #'org-gcal-extras--add-tag (string-split tags ":" 'omit-nulls)))
         ((listp tags)
          (mapcar #'org-gcal-extras--add-tag tags))
         (t
          (org-gcal-extras--add-tag tags)))))))

(defun org-gcal-extras--skip-event-by-summary-p (event)
  "Return nil to skip EVENT."
  (let ((summary (plist-get event :summary)))
    (not
     (cl-some
      (lambda (summary-to-skip)
        (string-match-p summary-to-skip summary))
      org-gcal-extras--summaries-to-skip))))

(provide 'org-gcal-extras)
;;; org-gcal-extras.el ends here

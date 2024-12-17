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
;; Package-Requires: ((emacs "24.3") (org-gcal "0.4.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

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
  (on-activate nil :type 'sexp :documentation "Run on profile activation."))

(defcustom org-gcal--current-profile nil
  "The current active profile, set in `org-gcal-activate-profile'."
  :type 'org-gcal-profile
  :group 'org-gcal)

(cl-defun org-gcal-activate-profile ((profile org-gcal-profile))
  "Set appropriate `org-gcal' variables based on PROFILE."
  (setq
   org-gcal--current-profile profile
   org-gcal-client-id (org-gcal-profile-client-id profile)
   org-gcal-client-secret (org-gcal-profile-client-secret profile)
   org-gcal-fetch-file-alist (org-gcal-profile-fetch-file-alist profile)
   org-gcal-after-update-entry-functions nil
   org-gcal-fetch-event-filters nil)
  (funcall (org-gcal-profile-on-activate profile))
  (dolist (fn (reverse (org-gcal-profile-after-update-entry-functions profile)))
    (add-hook 'org-gcal-after-update-entry-functions fn))
  (dolist (fn (reverse (org-gcal-profile-fetch-event-filters profile)))
    (add-hook 'org-gcal-fetch-event-filters fn))
  (when (fboundp 'org-gcal-reload-client-id-secret)
    (org-gcal-reload-client-id-secret)))

(provide 'org-gcal-extras)
;;; org-gcal-extras.el ends here

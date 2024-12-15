;;; dired-poke.el --- Quick directory navigation shortcuts -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx
;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: files, convenience, dired
;; URL: https://github.com/Laluxx/dired-poke

;;; Commentary:

;; Quick directory jumps for dired.
;; Press / followed by:
;; d -> Downloads
;; D -> Desktop
;; v -> Videos
;; p -> Pictures
;; m -> Music
;; c -> Documents
;; h -> Home

;;; Code:

(require 'dired)

(defgroup dired-poke nil
  "Quick directory navigation shortcuts."
  :group 'files
  :prefix "dired-poke-")

(defcustom dired-poke-alist
  `((?d . ,(expand-file-name "~/Downloads"))
    (?D . ,(expand-file-name "~/Desktop"))
    (?v . ,(expand-file-name "~/Videos"))
    (?p . ,(expand-file-name "~/Pictures"))
    (?m . ,(expand-file-name "~/Music"))
    (?c . ,(expand-file-name "~/Documents"))
    (?h . ,(expand-file-name "~/")))
  "Alist of quick directory shortcuts."
  :type '(alist :key-type character :value-type directory)
  :group 'dired-poke)

(defvar dired-poke-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding dired-poke-alist)
      (let ((key (car binding)))
        (define-key map (string key)  ; Use string to preserve case
                    `(lambda ()
                       (interactive)
                       (dired ,(cdr binding))))))
    map)
  "Keymap for dired-poke shortcuts.")

;;;###autoload
(defun dired-poke-to (char)
  "Jump to directory associated with CHAR in `dired-poke-alist'."
  (interactive "cDirectory shortcut: ")
  (if-let ((dir (alist-get char dired-poke-alist)))
      (dired dir)
    (user-error "No directory associated with '%c'" char)))

;; Add to dired-mode-map with case-sensitivity
(define-key dired-mode-map (kbd "/") dired-poke-map)

(provide 'dired-poke)

;;; dired-poke.el ends here

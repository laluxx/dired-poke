;;; dired-poke.el --- Quick directory navigation shortcuts -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx
;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: files, convenience, dired
;; URL: https://github.com/Laluxx/dired-poke

;;; Commentary:

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

(defun dired-poke-update-map ()
  "Update the keymap with current `dired-poke-alist' bindings."
  (let ((map (make-sparse-keymap)))
    (dolist (binding dired-poke-alist)
      (let ((key (car binding)))
        (define-key map (string key)
                    `(lambda ()
                       (interactive)
                       (dired ,(cdr binding))))))
    (define-key dired-mode-map (kbd "/") map)))

(defvar dired-poke-map (make-sparse-keymap)
  "Keymap for dired-poke shortcuts.")

;;;###autoload
(defun dired-poke-to (char)
  "Jump to directory associated with CHAR in `dired-poke-alist'."
  (interactive "cDirectory shortcut: ")
  (if-let ((dir (alist-get char dired-poke-alist)))
      (dired dir)
    (user-error "No directory associated with '%c'" char)))

(add-hook 'dired-mode-hook #'dired-poke-update-map)

(provide 'dired-poke)

;;; dired-poke.el ends here

;;; company-eshell-autosuggest.el --- History autosuggestions for eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/company-eshell-autosuggest
;; Git-Repository: git://github.com/dieggsy/company-eshell-autosuggest.git
;; Created: 2017-10-28
;; Version: 1.2.1
;; Keywords: completion company matching convenience abbrev
;; Package-Requires: ((emacs "24.4") (company "0.9.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides a company backend that implements functionality similar to fish
;; shell history autosuggestions.

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-eshell-autosuggest nil
  "Fish-like autosuggestions for eshell."
  :group 'company)

(defcustom company-eshell-autosuggest-delay 0
  "Delay for history autosuggestion."
  :group 'company-eshell-autosuggest
  :type 'number)

(defcustom company-eshell-autosuggest-use-company-map nil
  "Instead of overriding `company-active-map', use as-is.

This is disabled by default, as bindings in `company-active-map'
to RET and TAB may interfere with command input and completion
respectively."
  :group 'company-eshell-autosuggest
  :type 'boolean)

(defvar company-eshell-autosuggest-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<right>") 'company-complete-selection)
    keymap)
  "Keymap that is enabled during an active history
  autosuggestion.")

(defun company-eshell-autosuggest-candidates (prefix)
  "Select the first eshell history candidate with prefix PREFIX."
  (let* ((history
          (delete-dups
           (mapcar (lambda (str)
                     (string-trim (substring-no-properties str)))
                   (ring-elements eshell-history-ring))))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar))))

(defun company-eshell-autosuggest--prefix ()
  "Get current eshell input."
  (let* ((input-start (progn
                        (save-excursion
                          (beginning-of-line)
                          (while (not (looking-at-p eshell-prompt-regexp))
                            (forward-line -1))
                          (eshell-bol))))
         (prefix
          (string-trim-left
           (buffer-substring-no-properties
            input-start
            (line-end-position)))))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

;;;###autoload
(defun company-eshell-autosuggest (command &optional arg &rest ignored)
  "`company-mode' backend to provide eshell history suggestion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-autosuggest))
    (prefix (and (eq major-mode 'eshell-mode)
                 (company-eshell-autosuggest--prefix)))
    (candidates (company-eshell-autosuggest-candidates arg))))

(define-minor-mode company-eshell-autosuggest-mode
  "Enable fish-like autosuggestions in eshell.

You can use <right> to select the suggestion. This is
customizable through `company-eshell-autosuggest-active-map'. If
you prefer to use the default value of `company-active-map', you
may set the variable
`company-eshell-autosuggest-use-company-map', though this isn't
recommended as RET and TAB may not work as expected (send input,
trigger completions, respectively) when there is an active
suggestion.

The delay defaults to 0 seconds to emulate fish shell's
instantaneous suggestions, but is customizable with
`company-eshell-autosuggest-delay'.

Note: This assumes you want to use something other than company
for shell completion, e.g. `eshell-pcomplete',
`completion-at-point', or helm-esh-pcomplete, since
`company-active-map', `company-backends', and `company-frontends'
will be locally overriden and company will be used solely for
history autosuggestions."
  :init-value nil
  :group 'company-eshell-autosuggest
  (if company-eshell-autosuggest-mode
      (progn
        (company-mode 1)
        (unless company-eshell-autosuggest-use-company-map
          (setq-local company-active-map company-eshell-autosuggest-active-map))
        (setq-local company-idle-delay company-eshell-autosuggest-delay)
        (setq-local company-backends '(company-eshell-autosuggest))
        (setq-local company-frontends '(company-preview-frontend)))
    (company-mode -1)
    (kill-local-variable 'company-active-map)
    (kill-local-variable 'company-idle-delay)
    (kill-local-variable 'company-backends)
    (kill-local-variable 'company-frontends)))

(provide 'company-eshell-autosuggest)

;;; company-eshell-autosuggest.el ends here

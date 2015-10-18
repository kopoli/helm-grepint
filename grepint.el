;;; grepint.el --- Generic interface to grep -*- lexical-binding: t -*-

;; Copyright (C) 2015 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Maintainer: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 19 Sep 2015
;; Keywords: grep grepping
;; Version: 0.5.2
;; Package-Requires: ((helm "1.0") (emacs "24"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ### Description

;; This package solves the following problem for me:
;; - A single function call interface to grep and therefore keybinding.
;; - Selects the grep based on context: Inside a git-repository, runs
;;   git-grep, otherwise runs ag.
;; - Uses helm to select candidates and jumps to the given line with RET.

;; The following enables the aforementioned:

;;         (require 'grepint)
;;         (grepint-set-default-config)
;;         (global-set-key (kbd "C-c g") #'grepint-grep)

;; ### Additional features

;; This has a second interactive function `grepint-grep-root'. This runs the
;; grepping inside a root directory. By default this has been defined for the
;; git-grep where it greps from the git root directory.

;; ### Customization

;; Look into the function `grepint-set-default-config' to see how the default
;; cases are configured.

;; ### TODO / Wishlist

;; - Ability to swoop (i.e. retain the helm session while jumping).
;; - Better documentation.

;;; Code:

(require 'helm)
(require 'helm-utils)
(require 'helm-grep)
(require 'thingatpt)

(defcustom grepint-grep-list ()
  "List of grep commands.

These are the names in `grepint-grep-configs'."
  :group 'grepint)

(defcustom grepint-pre-input-function
  (lambda ()
    (if (region-active-p)
	(buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'symbol)))
  "The function that supplies the pre-input for grep."
  :group 'grepint)

(defvar grepint-grep-configs ()
  "Manipulate this with `grepint-add-grep-config'.")

(defmacro grepint-add-grep-config (name &rest configuration)
  "Add configuration NAME with properties from CONFIGURATION."
  `(progn (assq-delete-all ',name  grepint-grep-configs)
	  (push (cons ',name ',configuration) grepint-grep-configs)))

(defun grepint-get-grep-config (name)
  "Get the configuration associated with NAME."
  (assoc name grepint-grep-configs))

(defun grepint-grep-config-property (name property &rest new-value)
  "Get a config NAME's PROPERTY or set it to NEW-VALUE.
The config NAME has been added with `grepint-add-grep-config'.
Returns the current value of the property or nil if either name
or property was not found."
  (let ((cmd (assoc name grepint-grep-configs)))
    (when cmd
      (if (null new-value)
	  (plist-get (cdr cmd) property)
	(plist-put (cdr cmd) property (car new-value))
	(car new-value)))))

(defun grepint-run-command (&rest plist)
  "Run a grep command from PLIST.

The command line is constructed with the following PLIST items:

:command :arguments :extra-arguments.

The :arguments is split on whitespace, but :extra-arguments are
used as is."
  (let ((cmd (executable-find (plist-get plist :command))) proc)
    (when cmd
      (setq proc (apply 'start-process "grepint" nil
			(append
			 (list cmd)
			 (split-string (plist-get plist :arguments))
			 (list (plist-get plist :extra-arguments)))))
      (set-process-sentinel proc
			    (lambda (process event)
			      (helm-process-deferred-sentinel-hook process event (helm-default-directory))))
      proc)))


(defun grepint-select-grep ()
  "Select the grep based on :enable-function from `grepint-grep-configs'.

The greps are compared in order of `grepint-grep-list'.  If the
grep does not have :enable-function property, select it
automatically."
  (let (name enabler (greps grepint-grep-list))
    (while greps
      (setq name (car greps))
      (setq enabler (or (grepint-grep-config-property name :enable-function)
			#'(lambda () t)))
      (if (and (funcall enabler)
	       (executable-find (grepint-grep-config-property name :command)))
	  (setq greps nil)
	(setq name nil)
	(pop greps)))
    (when (not name)
      (error "Grepint: No suitable grep found"))
    name))

(defun grepint-grep-default-root ()
  "Get the default root directory if :root-directory-function isn't defined."
  default-directory)

;; Helm interface
(defvar grepint-grep-jump-pre-hook '(push-mark)
  "Hook that is run before jumping to the target in `grepint-grep-action-jump'.")
(defvar grepint-grep-jump-post-hook nil
  "Hook that is run after jumping to the target in `grepint-grep-action-jump'.")

(defun grepint-grep-parse-line (line)
  "Parse a LINE of output from grep-compatible programs.

Returns a list of (file line contents) or nil if the line could not be parsed."
  ;; The regexp gotten from helm-grep.el
  (let ((ret (string-match "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)"
			   line)))
    (if ret
	(mapcar #'(lambda (x) (match-string x line)) '(1 2 3)))))

(defun grepint-grep-action-jump (candidate)
  "Jump to line in a file described by a grep -line CANDIDATE."
  (run-hooks 'grepint-grep-jump-pre-hook)
  (let ((items (grepint-grep-parse-line candidate)))
    (find-file (nth 0 items))
    (helm-goto-line (string-to-number (nth 1 items))))
  (run-hooks 'grepint-grep-jump-post-hook))

(defun grepint-grep-process ()
  "This is the candidates-process for `grepint-helm-source'."
  (let ((cfg (grepint-get-grep-config (grepint-select-grep))))
    (apply #'grepint-run-command
	   :extra-arguments (replace-regexp-in-string "  *" ".*" helm-pattern)
	   (cdr cfg))))

(defun grepint-grep-filter-one-by-one (candidate)
  "Propertize each CANDIDATE provided by `grepint-helm-source'.

Uses `helm-grep-highlight-match' from helm-grep to provide line highlight."
  (let ((items (grepint-grep-parse-line candidate)))
    (if items
	(format "%s:%s:%s"
		(propertize (nth 0 items) 'face compilation-info-face)
		(propertize (nth 1 items) 'face compilation-line-face)
		(helm-grep-highlight-match (nth 2 items) t))
      "")))

(defvar grepint-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<right>") 'helm-execute-persistent-action)
    map))

(define-helm-type-attribute 'grepint
  `((volatile)
    (delayed)
    (requires-pattern . 3)
    (default-directory . nil)))

(defvar grepint-helm-source
  '((name . "Generic grep interface")
    (candidates-process . grepint-grep-process)
    (type . grepint)
    (action . (("Jump to" . grepint-grep-action-jump)))
    (candidate-number-limit . 500)
    (filter-one-by-one . grepint-grep-filter-one-by-one)))

(defun grepint--grep (in-root)
  "Run grep either in current directory or if IN-ROOT, in a root directory..

The grep function is determined by the contents of
`grepint-grep-configs' and the order of `grepint-grep-list'.  The
root directory is determined by the :root-directory-function
property of an element of `grepint-grep-configs'."
  (let ((name (grepint-select-grep))
	(default-directory default-directory))
    (when in-root
      (setq default-directory
	    (funcall (or (grepint-grep-config-property name :root-directory-function)
			 #'grepint-grep-default-root))))
    (helm :sources '(grepint-helm-source)
	  :buffer (format "Grepint%s: %s" (if in-root "-root" "") name)
	  :keymap grepint-helm-map
	  :input (funcall grepint-pre-input-function))))

;;;###autoload
(defun grepint-grep ()
  "Run grep in the current directory.

The grep function is determined by the contents of
`grepint-grep-configs' and the order of `grepint-grep-list'."
  (interactive)
  (grepint--grep nil))

;;;###autoload
(defun grepint-grep-root ()
  "This function is the same as `grepint-grep', but it runs the grep in a root directory."
  (interactive)

  (grepint--grep t))

;;;###autoload
(defun grepint-set-default-config ()
  "Set the default grep configuration into `grepint-grep-configs' and `grepint-grep-list'."

  (setq grepint-grep-configs nil)

  (defun grepint-git-grep-locate-root ()
    (locate-dominating-file (file-name-as-directory
			     (expand-file-name (file-truename default-directory)))
			    ".git"))

  (grepint-add-grep-config git-grep
			   :command "git"
			   :arguments "--no-pager grep --line-number --no-color"
			   :enable-function grepint-git-grep-locate-root
			   :root-directory-function grepint-git-grep-locate-root)

  (grepint-add-grep-config ag
			   :command "ag"
			   :arguments "--nocolor --ignore-case --search-zip --nogroup")

  (setq grepint-grep-list '(git-grep ag)))

(provide 'grepint)
;;; grepint.el ends here

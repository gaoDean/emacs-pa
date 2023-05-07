;;; emacs-pa.el --- A simple password manager -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dean Gao - MIT License
;; Author: Dean Gao <gao.dean@hotmail.com>
;; Description: Inline display of remote images in org-mode
;; Homepage: https://github.com/gaoDean/org-imgtog
;; Package-Requires: ((emacs "25.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This is a [[https://github.com/biox/pa][pa]] clone in emacs.
;; Requires [[https://github.com/FiloSottile/age][age]].

;;; Code:

(defun pa--rand-chars (length)
  "Generate a string of random chars from /dev/urandom of length LENGTH"
  (shell-command-to-string
   (concat "my/gen() {LC_ALL=C tr -dc \"$2\" </dev/urandom |
dd ibs=1 obs=1 count=\"$1\" 2>/dev/null};
my/gen"
           " "
           (format "%d" length)
           " "
           "_A-Z-a-z-0-9")))

(defun pa--generate-password ()
  "Generate a random password"
  (pa--rand-chars 50))

(defun pa--read-two-passwords ()
  (let ((pass1 (read-passwd "Enter password: "))
        (pass2 (read-passwd "Enter password (again): ")))
    (if (equal pass1 pass2)
        pass1
      nil)))

(defun pa--add (site account password)
  "Adds a password in the form SITE:ACCOUNT.age with PASSWORD.
Stored in ~/.local/share/pa/passwords to be compatible with biox/pa"
  (interactive (list (read-from-minibuffer "Site: ")
                     (read-from-minibuffer "Account: ")
                     (if (not (y-or-n-p "Generate a password?"))
                         (pa--read-two-passwords)
                       (pa--generate-password))))
  (when (and site
             (not (string-match-p ":" site))
             (not (string-match-p ":" account))
             (not (string-match-p "\\s-" site))
             (not (string-match-p "\\s-" account)))
    (if password
        (progn (shell-command (concat "age --encrypt -R ~/.local/share/pa/recipients -o "
                                      "~/.local/share/pa/passwords/"
                                      site
                                      ":"
                                      account
                                      ".age"
                                      " <<-EOF &&\n"
                                      password))
               (message (concat "Saved " site " <" account ">")))
      (message "Passwords did not match"))))

(defun pa--exclude-files (files exclude-files)
  (seq-filter (lambda (file) (not (member file exclude-files))) files))

(defun pa--list ()
  (let* ((directory "~/.local/share/pa/passwords")
         (exclude-files '("identities"
                          "recipients"
                          "."
                          ".."
                          ".age"
                          ".git"))
         (all-files (directory-files directory))
         (filtered-files (pa--exclude-files all-files exclude-files))
         (renamed-files (mapcar (lambda (filename)
                                  (replace-regexp-in-string (regexp-quote "\.age")
                                                            ""
                                                            filename))
                                filtered-files))
         (alist-files (mapcar (lambda (s)
                                (let ((key-value (split-string s ":")))
                                  (cons (car key-value) (cadr key-value))))
                              renamed-files)))
    alist-files))

(defun pa--padding-func-1 (str1 str2)
  "Complete right-align"
  (make-string
   (max 0 (- (window-width) (length str1) (length str2)))
   ?\s))

(defun pa--padding-func-2 (str1 _str2)
  "Relative left-align"
  (make-string
   (+ 3 (- max-candidate-length (length str1)))
   ?\s))

(defun pa--get-account (prompt alist)
  "Complete right-align"
  (let* ((max-candidate-length
          (apply #'max (mapcar (lambda (pair) (length (car pair))) alist)))
         (padded-alist
          (mapcar (lambda (pair)
                    (let* ((candidate (car pair))
                           (counterpart (cdr pair))
                           (padding (pa--padding-func-1 candidate counterpart)))
                      (cons (concat candidate padding counterpart) candidate)))
                  alist))
         (result (string-split (completing-read prompt padded-alist) "\s+")))
    result))

(defun pa--show (site account)
  "Copies the password to the clipboard"
  (interactive (pa--get-account "Account: " (pa--list)))
  (when (and site account)
    (kill-new (shell-command-to-string (concat "age --decrypt -i ~/.local/share/pa/identities "
                                               "~/.local/share/pa/passwords/"
                                               site
                                               ":"
                                               account
                                               ".age")))
    (message "Copied password to clipboard")))

(defun pa--edit (site account password)
  "Edits the password"
  (interactive (append (pa--get-account "Account: " (pa--list))
                       (list (if (not (y-or-n-p "Generate a password?"))
                                 (pa--read-two-passwords)
                               (pa--generate-password)))))
  (if (and site account (yes-or-no-p (concat "Confirm edit: "
                                             site
                                             " <"
                                             account
                                             ">?")))
      (if password
          (pa--add site account password)
        (message "Passwords did not match"))
    (message "Aborted")))

(defun pa--delete (site account)
  "Deletes the account"
  (interactive (pa--get-account "Account: " (pa--list)))
  (if (and site account (yes-or-no-p (concat "Confirm delete: "
                                               site
                                               " <"
                                               account
                                               ">?")))
    (delete-file (concat "~/.local/share/pa/passwords/"
                         site
                         ":"
                         account
                         ".age"))
    (message "Aborted")))

(defun pa--rename (site account new-site new-account)
  "Renames SITE and ACCOUNT to NEW-SITE and NEW-ACCOUNT respectively"
  (interactive (let* ((site-account (pa--get-account "Account: " (pa--list)))
                      (old-site (nth 0 site-account))
                      (old-account (nth 1 site-account))
                      (new-site
                       (read-from-minibuffer "New site name: " old-site))
                      (new-account
                       (read-from-minibuffer "New account name: " old-account)))
                 (list old-site old-account new-site new-account)))
  (if (and site account new-site new-account (yes-or-no-p (concat "Confirm rename: "
                                                                  site
                                                                  " <"
                                                                  account
                                                                  "> to "
                                                                  new-site
                                                                  " <"
                                                                  new-account
                                                                  ">?")))
      (rename-file (concat "~/.local/share/pa/passwords/" site ":" account ".age")
                   (concat "~/.local/share/pa/passwords/" new-site ":" new-account ".age"))
    (message "Aborted")))

(provide 'emacs-pa)

;;; emacs-pa.el ends here

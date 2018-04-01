;;; pwstore.el --- password manager                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Stephen Nutt

;; Author: Stephen Nutt <stnutt@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'plstore)

(defgroup pwstore nil
  ""
  :prefix "pwstore-"
  :group 'pwstore)

(defconst pwstore-numbers "0123456789"
  "A string containing all numbers suitable for passwords.")

(defconst pwstore-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "A string containing all uppercase letters suitable for passwords.")

(defconst pwstore-lower "abcdefghijklmnopqrstuvwxyz"
  "A string containing all lowercase letters suitable for passwords.")

(defconst pwstore-symbols ""
  "A string containing all symbols suitable for passwords.")

(defcustom pwstore-file (locate-user-emacs-file "pwstore.plist")
  "The file containing the password store."
  :type 'file
  :group 'pwstore)

(defcustom pwstore-encrypt-to nil
  "Key ID used for encrypting the store."
  :type 'string
  :group 'pwstore)

(defcustom pwstore-length 16
  "The default length for generated passwords."
  :type 'integer
  :group 'pwstore)

(defcustom pwstore-characters (concat pwstore-numbers
                                      pwstore-upper
                                      pwstore-lower
                                      pwstore-symbols)
  "A string containing the characters used to generate passwords."
  :type 'string
  :group 'pwstore)

(defcustom pwstore-password-key :secret
  ""
  :type 'symbol
  :group 'pwstore)

(defcustom pwstore-username-key :user
  ""
  :type 'symbol
  :group 'pwstore)

(defcustom pwstore-url-key :url
  ""
  :type 'symbol
  :group 'pwstore)

(defcustom pwstore-keys '(:host :port)
  ""
  :type '(repeat symbol)
  :group 'pwstore)

(defcustom pwstore-secret-keys nil
  ""
  :type '(repeat symbol)
  :group 'pwstore)

(defcustom pwstore-timeout 45
  "The time in seconds until the password is cleared from the kill ring."
  :type 'integer
  :group 'pwstore)

(defvar pwstore-timer nil)

(defmacro pwstore-with-store (&rest body)
  "Execute the forms in BODY with the open store bound to `store'."
  (declare (indent 0))
  `(let ((store (plstore-open pwstore-file))
         (plstore-encrypt-to (or pwstore-encrypt-to plstore-encrypt-to)))
     (unwind-protect
         (progn
           ,@body)
       (plstore-close store))))

(defun pwstore-names ()
  "Return a list of all names in the store."
  (when (file-exists-p pwstore-file)
    (with-temp-buffer
      (insert-file-contents pwstore-file)
      (unless (= (buffer-size) 0)
        (sort (mapcar 'car (read (current-buffer))) 'string-lessp)))))

(defun pwstore-read-name (&optional require-match)
  ""
  (let ((names (pwstore-names)))
    (cond
     (names (completing-read "Name: " names nil require-match))
     (require-match (user-error "Store is empty!"))
     (t (read-string "Name: ")))))

(defun pwstore-read-key ()
  ""
  (let* ((keys (mapcar (lambda (key)
                         (substring (symbol-name key) 1))
                       (nconc (list pwstore-password-key
                                    pwstore-username-key
                                    pwstore-url-key)
                               pwstore-keys
                               pwstore-secret-keys)))
         (key (completing-read "Key: " keys)))
    (unless (string-empty-p key)
      (intern (concat ":" key)))))

(defun pwstore-read-value (key)
  ""
  (let ((value (if (eq key pwstore-password-key)
                   (read-passwd "Password: " t)
                 (read-string "Value: "))))
    (unless (string-empty-p value)
      value)))

(defun pwstore-read-plist ()
  ""
  (cl-loop nconc (if-let ((key (pwstore-read-key)))
                     (list key (pwstore-read-value key))
                   (cl-return plist))
           into plist))

(defun pwstore-secret-p (key)
  ""
  (or (eq key pwstore-password-key)
      (not (null (memq key pwstore-secret-keys)))))

(defun pwstore-empty-p (store)
  ""
  (with-current-buffer (plstore--get-buffer store)
    (save-excursion
      (goto-char (point-min))
      (null (read (current-buffer))))))

(defun pwstore--subplist (plist props)
  ""
  (cl-loop for prop in props nconc (list prop (plist-get plist prop))))

(defun pwstore--keys (store name)
  ""
  (let (keys secret-keys)
    (with-current-buffer (plstore--get-buffer store)
      (unless (= 0 (buffer-size))
        (save-excursion
          (goto-char (point-min))
          (dolist (key (assoc name (read (current-buffer))))
            (when (keywordp key)
              (if (string-prefix-p ":secret-" (symbol-name key))
                  (push (thread-last key
                          (symbol-name)
                          (string-remove-prefix ":secret-")
                          (concat ":")
                          (intern))
                        secret-keys)
                (push key keys)))))))
    (vector (nreverse keys) (nreverse secret-keys))))

(defun pwstore-random (length characters)
  "Return a string of length LENGTH randomly chosen from the string CHARACTERS."
  (cl-loop repeat length
           concat (string (aref characters (random (length characters))))))

(defun pwstore--find-filter-args (args)
  ""
  (list (car args)
        (cl-loop with keys = (cadr args)
                 for i from 0 below (length keys) by 2
                 nconc (list (nth i keys)
                             (if (listp (nth (+ i 1) keys))
                                 (nth (+ i 1) keys)
                               (list (nth (+ i 1) keys)))))))

(advice-add 'plstore-find :filter-args 'pwstore--find-filter-args)

;;;###autoload
(defun pwstore-set (name &rest plist)
  ""
  (interactive (cons (pwstore-read-name) (pwstore-read-plist)))
  (pwstore-with-store
    (cl-loop with -keys = (pwstore--keys store name)
             with entry = (plstore-get store name)
             with keys = (thread-last plist
                           (cl-remove-if-not 'keywordp)
                           (cl-set-difference (aref -keys 0))
                           (pwstore--subplist (cdr entry)))
             with secret-keys = (thread-last plist
                                  (cl-remove-if-not 'keywordp)
                                  (cl-set-difference (aref -keys 1))
                                  (pwstore--subplist (cdr entry)))
             for i from 0 below (length plist) by 2
             do (when-let ((key (nth i plist))
                           (value (nth (+ i 1) plist)))
                  (when (and (eq key :port)
                             (stringp value)
                             (string-match-p "\\`[[:digit:]]+\\'" value))
                    (setq value (string-to-number value)))
                  (if (pwstore-secret-p key)
                      (setq secret-keys (plist-put secret-keys key value))
                    (setq keys (plist-put keys key value))))
             finally do (progn
                          (plstore-put store name keys secret-keys)
                          (plstore-save store)))))

(defun pwstore--get (store name key)
  ""
  (let ((keys (pwstore--keys store name)))
    (cond
     ((memq key (aref keys 0))
      (with-current-buffer (plstore--get-buffer store)
        (save-excursion
          (goto-char (point-min))
          (plist-get (cdr (assoc name (read (current-buffer)))) key))))
     ((memq key (aref keys 1))
      (plist-get (cdr (plstore-get store name)) key))
     (t nil))))

;;;###autoload
(defun pwstore-get (name key)
  ""
  (interactive (list (pwstore-read-name t) (pwstore-read-key)))
  (let ((value (pwstore-with-store
                 (pwstore--get store name key))))
    (cond
     ((not (called-interactively-p 'interactive)) value)
     (value (let ((message-log-max nil))
              (message "%s" value)))
     (t (user-error "")))))

;;;###autoload
(defun pwstore-insert (name password)
  "Add NAME with PASSWORD to the store."
  (interactive (list (pwstore-read-name) (pwstore-read-value pwstore-password-key)))
  (pwstore-set name pwstore-password-key password))

;;;###autoload
(defun pwstore-generate (name)
  "And NAME with a randomly-generated password to the store."
  (interactive (list (pwstore-read-name)))
  (pwstore-insert name (pwstore-random pwstore-length pwstore-characters)))

;;;###autoload
(defun pwstore-remove (name)
  "Remove NAME from the store."
  (interactive (list (pwstore-read-name t)))
  (unless (and (called-interactively-p 'interactive)
               (not (yes-or-no-p (format "Remove \"%s\"?"  name))))
    (pwstore-with-store
      (when (plstore-get store name)
        (plstore-delete store name)
        (plstore-save store)
        (when (pwstore-empty-p store)
          (delete-file pwstore-file))))))

;;;###autoload
(defun pwstore-copy (name new-name)
  ""
  (interactive (list (pwstore-read-name t) (pwstore-read-name)))
  (pwstore-with-store
    (let* ((-keys (pwstore--keys store name))
           (entry (plstore-get store name))
           (keys (pwstore--subplist (cdr entry) (aref -keys 0)))
           (secret-keys (pwstore--subplist (cdr entry) (aref -keys 1))))
      (plstore-put store new-name keys secret-keys)
      (plstore-save store))))

;;;###autoload
(defun pwstore-rename (name new-name)
  "Rename NAME to NEW-NAME in the store."
  (interactive (list (pwstore-read-name t) (pwstore-read-name)))
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p (format "Rename \"%s\" to \"%s\"" name new-name)))
    (pwstore-copy name new-name)
    (pwstore-remove name)))

;;;###autoload
(defun pwstore-browse (name)
  "Call `browse-url' on the URL for NAME."
  (interactive (list (pwstore-read-name t)))
  (if-let ((url (pwstore-get name pwstore-url-key)))
      (browse-url url)
    (user-error "")))

(defun pwstore-login (name)
  ""
  (interactive (list (pwstore-read-name t)))
  (condition-case nil
      (pwstore-browse name)
    (user-error))
  (pwstore-kill name pwstore-username-key pwstore-password-key))

(defun pwstore-clear ()
  ""
  (interactive)
  (remove-hook 'x-sent-selection-functions 'pwstore-x-sent-selection)
  (when pwstore-timer
    (setq pwstore-timer (cancel-timer pwstore-timer)))
  (dolist (selection '(CLIPBOARD PRIMARY))
    (when (get-text-property 0 'pwstore-name (or (gui-get-selection selection) ""))
      (gui-set-selection selection nil)))
  (setq kill-ring (cl-remove-if (apply-partially 'get-text-property 0 'pwstore-name) kill-ring)))

;;;###autoload
(defun pwstore-kill (name &rest keys)
  ""
  (interactive (nconc (list (pwstore-read-name t))
                      (when current-prefix-arg
                        (list (pwstore-read-key)))))
  (unless keys
    (setq keys (list pwstore-password-key)))
  (pwstore-clear)
  (if-let ((value (pwstore-get name (car keys))))
      (progn
        (kill-new (propertize value 'pwstore-name name 'pwstore-keys keys))
        (setq pwstore-timer (run-at-time pwstore-timeout nil 'pwstore-clear))
        (when (cdr keys)
          (add-hook 'x-sent-selection-functions 'pwstore-x-sent-selection)))
    (user-error "")))

;;;###autoload
(defun pwstore-edit ()
  "Edit the `pwstore-file' in `plstore-mode'."
  (interactive)
  (unless (file-exists-p pwstore-file)
    (user-error "File does not exist: %s" pwstore-file))
  (when-let ((buffer (find-buffer-visiting (file-truename pwstore-file))))
    (unless (eq (buffer-local-value 'major-mode buffer) 'plstore-mode)
      (kill-buffer buffer)))
  (find-file pwstore-file)
  (when pwstore-encrypt-to
    (setq-local plstore-encrypt-to pwstore-encrypt-to)))

;; TODO save only after import finished

(defun pwstore-import-pass (directory)
  ""
  (interactive (list (read-directory-name "Directory: " "~")))
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (with-temp-buffer
    (dolist (file (directory-files-recursively directory "\\.gpg\\'"))
      (erase-buffer)
      (insert-file-contents file)
      (goto-char (point-min))
      (cl-loop with name = (file-name-sans-extension (string-remove-prefix directory file))
               with password = (buffer-substring (point) (point-at-eol))
               initially do (forward-line 1)
               while (re-search-forward "^\\(.*?\\):[[:blank:]]*\\(.*\\)$" nil t)
               nconc (list (intern (concat ":" (downcase (match-string 1)))) (match-string 2))
               into plist
               finally do (apply 'pwstore-set name pwstore-password-key password plist)))))

(declare-function netrc-parse "netrc")
(declare-function netrc-get "netrc")

(defun pwstore-import-netrc (file)
  "Import a FILE in .netrc format."
  (interactive (list (read-file-name "File: " nil nil nil
                                     (cl-find-if 'file-exists-p
                                                 '("~/.authinfo"
                                                   "~/.authinfo.gpg"
                                                   "~/.netrc")))))
  (require 'netrc)
  (dolist (alist (netrc-parse file))
    (let ((password (netrc-get alist "password"))
          (host (netrc-get alist "machine"))
          (user (netrc-get alist "login"))
          (port (netrc-get alist "port")))
      (let ((name (concat (when user (concat user "@"))
                          (when host host)
                          (when port (concat ":" port)))))
        (pwstore-set name pwstore-password-key password :host host :user user :port port)))))

(defun pwstore-x-sent-selection (type &rest _)
  ""
  (when-let ((selection (gui-get-selection type))
             (name (get-text-property 0 'pwstore-name selection))
             (keys (cdr (get-text-property 0 'pwstore-keys selection))))
    (cancel-timer pwstore-timer)
    (setq pwstore-timer (run-with-timer 0.5 nil 'apply 'pwstore-kill name keys))))

(defun pwstore-unload-function ()
  ""
  (advice-remove 'plstore-find 'pwstore--find-filter-args))

(provide 'pwstore)
;;; pwstore.el ends here


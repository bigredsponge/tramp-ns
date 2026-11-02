;;; tramp-ns.el --- TRAMP backend for user-owned Linux namespaces -*- lexical-binding: t; -*-

;; Copyright (c) 2026 bigredsponge

;; Author: bigredsponge
;; URL: https://github.com/bigredsponge/tramp-ns
;; Version: 0.1.0
;; Keywords: comm, processes, files
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: MIT

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; tramp-ns is a TRAMP backend for user-owned Linux namespaces, such as
;; rootless Podman containers.  It reads and writes files through
;; /proc/PID/root on the host, and runs commands inside the namespace
;; with nsenter.  File access uses the host's credentials; programs
;; retain those credentials.  It requires Linux and a util-linux nsenter
;; that supports --wdns.
;;
;; Add entries to `tramp-ns-environments', each a name and an :enter
;; command:
;;
;;   (setq tramp-ns-environments
;;         '(("mypod" :enter ("podman" "exec" "-i" "mypod"))
;;           ("mybox" :enter ("distrobox" "enter" "mybox" "--"))))
;;
;; Then visit /ns:mypod:/path/to/file.  See README.md for details.

;;; Code:

(require 'tramp-sh)
(require 'cl-lib)

(defgroup tramp-ns nil "Local namespace environments."
  :prefix "tramp-ns-"
  :group 'tramp)

(defcustom tramp-ns-environments nil
  "Alist of (NAME :enter (PROGRAM ARG...)) environment definitions.
The command prefix must run its remaining arguments inside the environment,
with stdin open, without allocating a terminal.  Optional :timeout overrides
`tramp-ns-start-timeout' for this environment.  For example:
  ((\"box\" :enter (\"podman\" \"exec\" \"-i\" \"box\")))"
  :type '(alist :key-type string :value-type plist)
  :group 'tramp-ns)

(defcustom tramp-ns-start-timeout 120
  "Seconds to wait for an environment to start."
  :type 'number
  :group 'tramp-ns)

(defun tramp-ns--read (file)
  "Read FILE literally."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun tramp-ns--connection (vec)
  "Return the live environment process for VEC, starting it if needed."
  (or (let ((p (tramp-get-connection-process vec)))
        (and p (process-live-p p) (process-get p 'tramp-ns-pid) p))
      (progn
        (unless (tramp-connectable-p vec)
          (throw 'non-essential 'non-essential))
        (let* ((entry (assoc (tramp-file-name-host vec) tramp-ns-environments))
               (prefix (plist-get (cdr entry) :enter))
               (marker (concat "tramp-ns-" (make-temp-name "")))
               (command (list "sh" "-c" "read _" marker))
               (expected (concat (mapconcat #'identity command "\0") "\0"))
               (default-directory "/")
               (deadline (+ (float-time) (or (plist-get (cdr entry) :timeout)
                                            tramp-ns-start-timeout)))
               (reporter (make-progress-reporter
                          (format "Entering %s..." (car entry))))
               p pid)
          (unless prefix (error "No :enter command for %s" (car entry)))
          (setq p (make-process
                   :name (tramp-get-connection-name vec)
                   :buffer (tramp-get-connection-buffer vec)
                   :command (append prefix command)
                   :connection-type 'pipe :noquery t))
          (unwind-protect
              (progn
                (while (and (not pid) (process-live-p p)
                            (< (float-time) deadline))
                  (dolist (dir (directory-files "/proc" t "\\`[0-9]+\\'"))
                    (when (equal (ignore-errors
                                   (tramp-ns--read (concat dir "/cmdline")))
                                 expected)
                      (setq pid (string-to-number (file-name-nondirectory dir)))))
                  (unless pid
                    (accept-process-output p 0.02)
                    (progress-reporter-update
                     reporter nil (with-current-buffer (process-buffer p)
                                    (car (last (split-string (buffer-string) "\n" t)))))
                    (redisplay)))
                (unless pid
                  (error "Environment did not start: %s"
                         (with-current-buffer (process-buffer p) (buffer-string))))
                (process-put p 'tramp-ns-pid pid)
                (tramp-set-connection-property
                 vec "login-args" (list (tramp-ns--login-args pid)))
                (process-put p 'tramp-ns-environment
                             (split-string
                              (tramp-ns--read (format "/proc/%d/environ" pid))
                              "\0" t))
                (tramp-post-process-creation p vec)
                (tramp-set-connection-property p "connected" t)
                (tramp-set-connection-local-variables vec)
                (progress-reporter-done reporter)
                p)
            (unless (process-get p 'tramp-ns-pid) (delete-process p)))))))

(defun tramp-ns--resolve (root path &optional nofollow)
  "Resolve PATH within ROOT, following links component by component.
Absolute link targets restart at ROOT; .. cannot escape it.
With NOFOLLOW, leave the final component unresolved."
  (let ((todo (split-string path "/" t)) done (links 0))
    (while todo
      (let ((part (pop todo)))
        (cond
         ((equal part "."))
         ((equal part "..") (pop done))
         (t
          (let* ((candidate (concat root "/"
                                    (mapconcat #'identity
                                               (reverse (cons part done)) "/")))
                 (target (unless (and nofollow (null todo))
                           (file-symlink-p candidate))))
            (if (not target) (push part done)
              (when (> (cl-incf links) 40)
                (signal 'file-error (list "Too many symbolic links" path)))
              (when (string-prefix-p "/" target) (setq done nil))
              (setq todo (append (split-string target "/" t) todo))))))))
    (concat root "/" (mapconcat #'identity (reverse done) "/")
            (when (and done (not nofollow) (string-suffix-p "/" path)) "/"))))

(defun tramp-ns--local (file &optional nofollow)
  "Translate an ns FILE to a host path, optionally with NOFOLLOW."
  (if (not (tramp-ns--file-name-p file)) file
    (let* ((vec (tramp-dissect-file-name file))
           (p (tramp-ns--connection vec)))
      (tramp-ns--resolve
       (format "/proc/%d/root" (process-get p 'tramp-ns-pid))
       (file-name-unquote (tramp-file-name-localname vec)) nofollow))))

(defun tramp-ns--file-operation (operation &rest args)
  "Run file OPERATION on translated ARGS."
  (let ((nofollow (memq operation '(file-attributes delete-file delete-directory
                                  rename-file make-directory))))
    (setcar args (tramp-ns--local (car args) nofollow))
    (when (memq operation '(copy-file rename-file add-name-to-file))
      (setcar (cdr args) (tramp-ns--local (cadr args) t)))
    (apply operation args)))

(defun tramp-ns--completions (file directory)
  "Return completions for FILE in DIRECTORY using TRAMP's filtering."
  (tramp-skeleton-file-name-all-completions file directory
    (all-completions
     file (mapcar (lambda (name)
                    (if (file-directory-p (concat directory name))
                        (concat name "/") name))
                  (directory-files (tramp-ns--local directory))))))

(defun tramp-ns--exec-path ()
  "Return the inner process's executable search path."
  (let* ((vec (tramp-dissect-file-name default-directory))
         (env (tramp-ns--environment (tramp-ns--connection vec))))
    (append (split-string (getenv-internal "PATH" env) ":")
            (list (tramp-file-name-localname vec)))))

(defun tramp-ns--insert (filename &optional visit beg end replace)
  "Insert FILENAME, honoring VISIT, BEG, END and REPLACE."
  (let (result)
    (unwind-protect
        (setq result (insert-file-contents
                      (tramp-ns--local filename) visit beg end replace))
      (when visit (setq buffer-file-name filename)))
    (cons filename (cdr result))))

(defun tramp-ns--write (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for START, END, FILENAME and APPEND.
VISIT, LOCKNAME and MUSTBENEW are as for `write-region'."
  (tramp-skeleton-write-region start end filename append visit lockname mustbenew
    (let (create-lockfiles)
      (write-region start end (tramp-ns--local filename) append 'nomessage))
    (setq coding-system-used last-coding-system-used)))

(defun tramp-ns--environment (p)
  "Return P's inner environment plus dynamically bound variables."
  (append (cl-remove-if
           (lambda (entry)
             (member entry (default-toplevel-value 'process-environment)))
           process-environment)
          (process-get p 'tramp-ns-environment)))

(defun tramp-ns--login-args (pid)
  "Return nsenter's arguments for joining PID's namespaces."
  (let ((base (format "/proc/%d/ns/" pid))
        (flags '(("user" . "--user") ("mnt" . "--mount")
                 ("uts" . "--uts") ("ipc" . "--ipc") ("net" . "--net")
                 ("pid" . "--pid") ("cgroup" . "--cgroup") ("time" . "--time"))))
    (append (list "-t" (number-to-string pid) "-r" "--preserve-credentials")
            (cl-loop for (name . flag) in flags
                     unless (equal (file-symlink-p (concat base name))
                                   (file-symlink-p (concat "/proc/self/ns/" name)))
                     collect flag)
            '("--"))))

(defun tramp-ns--process-file (program &optional infile destination display &rest args)
  "Run PROGRAM with INFILE, DESTINATION, DISPLAY and ARGS inside the environment."
  (let* ((vec (tramp-dissect-file-name default-directory))
         (p (tramp-ns--connection vec))
         (infile (and infile (tramp-ns--local infile)))
         (destination (if (and (consp destination) (stringp (cadr destination)))
                          (list (car destination) (tramp-ns--local (cadr destination)))
                        destination))
         (process-environment (tramp-ns--environment p))
         (default-directory "/"))
    (apply #'call-process (executable-find "nsenter") infile destination display
           (concat "--wdns=" (tramp-file-name-localname vec))
           (append (tramp-ns--login-args (process-get p 'tramp-ns-pid))
                   (list program) args))))

(defun tramp-ns--make-process (&rest args)
  "Like `tramp-handle-make-process' for ARGS, with the inner environment."
  (let ((process-environment (tramp-ns--environment
                              (tramp-ns--connection
                               (tramp-dissect-file-name default-directory)))))
    (apply #'tramp-handle-make-process args)))

(defun tramp-ns--home (vec &optional _user)
  "Return the environment's HOME for VEC."
  (getenv-internal "HOME" (process-get (tramp-ns--connection vec) 'tramp-ns-environment)))

(defconst tramp-ns-file-name-handler-alist
  (append
   (mapcar (lambda (op) (cons op (intern (concat "tramp-handle-" (symbol-name op)))))
           '(access-file copy-directory directory-file-name directory-files
             directory-files-and-attributes dired-uncache
             file-accessible-directory-p file-directory-p file-equal-p
             file-in-directory-p file-local-copy file-locked-p file-modes
             file-name-as-directory file-name-case-insensitive-p file-name-completion
             file-name-directory file-name-nondirectory file-newer-than-file-p
             file-regular-p file-remote-p file-symlink-p file-truename
             find-backup-file-name insert-directory load lock-file
             make-auto-save-file-name make-lock-file-name make-nearby-temp-file
             set-visited-file-modtime shell-command start-file-process
             substitute-in-file-name temporary-file-directory unlock-file
             verify-visited-file-modtime))
   (mapcar (lambda (op) (cons op #'tramp-ns--file-operation))
           '(add-name-to-file copy-file delete-directory delete-file file-attributes
             file-executable-p file-exists-p file-readable-p file-writable-p
             file-system-info make-directory rename-file set-file-modes set-file-times))
   '((expand-file-name . tramp-sh-handle-expand-file-name)
     (exec-path . tramp-ns--exec-path)
     (file-name-all-completions . tramp-ns--completions)
     (insert-file-contents . tramp-ns--insert) (write-region . tramp-ns--write)
     (process-file . tramp-ns--process-file) (make-process . tramp-ns--make-process)
     (tramp-get-home-directory . tramp-ns--home)
     (tramp-get-remote-uid . ignore) (tramp-get-remote-gid . ignore)
     (tramp-set-file-uid-gid . ignore) (file-acl . ignore)
     (file-selinux-context . tramp-handle-file-selinux-context)
     (set-file-acl . ignore) (set-file-selinux-context . ignore)
     (unhandled-file-name-directory . ignore) (vc-registered . ignore)))
  "Operations handled by the ns backend.")

(defun tramp-ns--file-name-p (file)
  "Whether FILE is an ns name or dissected TRAMP vector."
  (let ((vec (tramp-ensure-dissected-file-name file)))
    (and vec (equal (tramp-file-name-method vec) "ns"))))

(defun tramp-ns-file-name-handler (operation &rest args)
  "Dispatch OPERATION with ARGS through TRAMP's ns backend."
  (let ((handler (alist-get operation tramp-ns-file-name-handler-alist)))
    (save-match-data
      (cond ((eq handler #'tramp-ns--file-operation)
             (apply handler operation args))
            (handler (apply handler args))
            (t (tramp-run-real-handler operation args))))))

(defun tramp-ns--complete-environments (_file)
  "Return configured namespace environments for TRAMP completion."
  (mapcar (lambda (environment) (list nil (car environment)))
          tramp-ns-environments))

(add-to-list 'tramp-methods '("ns" (tramp-login-program "nsenter")
                            (tramp-direct-async ("/bin/sh" "-c"))
                            (tramp-remote-shell "/bin/sh")
                            (tramp-remote-shell-args ("-c"))))
(tramp-set-completion-function
 "ns" '((tramp-ns--complete-environments "")))
(tramp-register-foreign-file-name-handler #'tramp-ns--file-name-p
                                          #'tramp-ns-file-name-handler)

(provide 'tramp-ns)

;;;###autoload
(with-eval-after-load 'tramp
  (require 'tramp-ns))
;;; tramp-ns.el ends here

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
  :type '(alist :key-type (string :tag "Name")
                :value-type (plist :options ((:enter (repeat string))
                                             (:timeout number))))
  :group 'tramp-ns)

(defcustom tramp-ns-start-timeout 120
  "Seconds to wait for an environment to start."
  :type 'number
  :group 'tramp-ns)

;;;; Connections

(defun tramp-ns--read (file)
  "Read FILE literally."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun tramp-ns--find-pid (cmdline)
  "Return the host PID of the process whose command line is CMDLINE."
  ;; The entered process cannot report its own PID, since $$ would be
  ;; its PID inside the environment's PID namespace, not on the host.
  (seq-some (lambda (dir)
              (and (equal (ignore-errors (tramp-ns--read (concat dir "/cmdline")))
                          cmdline)
                   (string-to-number (file-name-nondirectory dir))))
            (directory-files "/proc" t "\\`[0-9]+\\'")))

(defun tramp-ns--definition (vec)
  "Return the plist that defines VEC's environment, after checking it."
  (let* ((host (tramp-file-name-host vec))
         (entry (assoc host tramp-ns-environments))
         (prefix (plist-get (cdr entry) :enter))
         (timeout (or (plist-get (cdr entry) :timeout) tramp-ns-start-timeout)))
    (cond ((or (tramp-file-name-user vec) (tramp-file-name-port vec)
               (tramp-file-name-hop vec))
           (tramp-user-error vec "An ns name takes no user, port or hop"))
          ((not entry)
           (tramp-user-error vec "No environment named %s" host))
          ((not (and (consp prefix) (seq-every-p #'stringp prefix)))
           (tramp-user-error vec ":enter for %s is not a list of strings" host))
          ((not (and (numberp timeout) (>= timeout 0)))
           (tramp-user-error vec "%s is not a number of seconds"
                             (if (plist-member (cdr entry) :timeout)
                                 (format ":timeout for %s" host)
                               "`tramp-ns-start-timeout'"))))
    (cdr entry)))

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

(defun tramp-ns--last-line (p)
  "Return the last nonempty line of P's output so far."
  (with-current-buffer (process-buffer p)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (buffer-substring-no-properties (line-beginning-position) (point)))))

(defun tramp-ns--start (vec)
  "Enter the environment named by VEC and return its process."
  (let* ((host (tramp-file-name-host vec))
         (definition (tramp-ns--definition vec))
         (prefix (plist-get definition :enter))
         (timeout (or (plist-get definition :timeout) tramp-ns-start-timeout))
         (marker (concat "tramp-ns-" (make-temp-name "")))
         (command (list "sh" "-c" "read _" marker))
         (cmdline (concat (mapconcat #'identity command "\0") "\0"))
         (deadline (+ (float-time) timeout))
         (reporter (make-progress-reporter (format "Entering %s..." host)))
         (default-directory "/")
         (p (make-process
             :name (tramp-get-connection-name vec)
             :buffer (tramp-get-connection-buffer vec)
             :command (append prefix command)
             :connection-type 'pipe :noquery t))
         (delay 0.01)
         pid started)
    (unwind-protect
        (progn
          (while (and (not (setq pid (tramp-ns--find-pid cmdline)))
                      (process-live-p p) (< (float-time) deadline))
            (accept-process-output p (min delay (max 0 (- deadline (float-time)))))
            (setq delay (min (* delay 2) 0.5))
            (progress-reporter-update reporter nil (tramp-ns--last-line p))
            (redisplay))
          (unless pid
            (tramp-error vec 'file-error "Environment %s did not start: %s" host
                         (with-current-buffer (process-buffer p) (buffer-string))))
          (process-put p 'tramp-ns-pid pid)
          (process-put p 'tramp-ns-cmdline cmdline)
          (process-put p 'tramp-ns-enter (copy-tree prefix))
          ;; `tramp-handle-make-process' reads the method's login args
          ;; through this cache, which lets them name the PID.
          (tramp-set-connection-property
           vec "login-args" (list (tramp-ns--login-args pid)))
          (process-put p 'tramp-ns-environment
                       (split-string
                        (tramp-ns--read (format "/proc/%d/environ" pid)) "\0" t))
          (tramp-post-process-creation p vec)
          (tramp-set-connection-property p "connected" t)
          (tramp-set-connection-local-variables vec)
          (progress-reporter-done reporter)
          (setq started t)
          p)
      (unless started (delete-process p)))))

(defun tramp-ns--live-p (p)
  "Whether P and the process it entered are both still running."
  (when-let* ((pid (and p (process-live-p p) (process-get p 'tramp-ns-pid))))
    ;; The command line, unlike the PID, cannot be reused by chance.
    (equal (ignore-errors (tramp-ns--read (format "/proc/%d/cmdline" pid)))
           (process-get p 'tramp-ns-cmdline))))

(defun tramp-ns--connection (vec)
  "Return the live environment process for VEC, starting it if needed."
  (let ((p (tramp-get-connection-process vec)))
    (cond ((and (tramp-ns--live-p p)
                ;; A changed :enter command names a different environment.
                (equal (process-get p 'tramp-ns-enter)
                       (plist-get (cdr (assoc (tramp-file-name-host vec)
                                              tramp-ns-environments))
                                  :enter)))
           p)
          ((not (tramp-connectable-p vec)) (throw 'non-essential 'non-essential))
          (t (when p (tramp-cleanup-connection vec t))
             (tramp-ns--start vec)))))

(defun tramp-ns--environment (p)
  "Return P's inner environment plus dynamically bound variables."
  ;; Compare by value: eshell, for one, binds fresh copies of the host's
  ;; PATH, which must not override the environment's.
  (append (cl-remove-if
           (lambda (entry)
             (member entry (default-toplevel-value 'process-environment)))
           process-environment)
          (process-get p 'tramp-ns-environment)))

(defun tramp-ns--home (vec &optional user)
  "Return USER's home directory in VEC's environment, by default HOME."
  (let ((p (tramp-ns--connection vec)))
    (if (tramp-string-empty-or-nil-p user)
        (or (getenv-internal "HOME" (process-get p 'tramp-ns-environment))
            (tramp-user-error vec "%s has no HOME" (tramp-file-name-host vec)))
      ;; getent, unlike reading /etc/passwd, also finds NSS accounts.
      (let ((default-directory (tramp-make-tramp-file-name vec "/"))
            process-file-side-effects)
        (with-temp-buffer
          (when (zerop (process-file "getent" nil t nil "passwd" user))
            (nth 5 (split-string (string-trim-right (buffer-string)) ":"))))))))

;;;; Paths

(define-error 'tramp-ns-link-loop "Too many levels of symbolic links" 'file-error)

(defun tramp-ns--file-name-p (file)
  "Whether FILE is an ns name or dissected TRAMP vector."
  (let ((vec (tramp-ensure-dissected-file-name file)))
    (and vec (equal (tramp-file-name-method vec) "ns"))))

(defun tramp-ns--join (root parts)
  "Return ROOT followed by PARTS, which are in reverse order."
  (concat root "/" (string-join (reverse parts) "/")))

(defun tramp-ns--resolve (root path &optional nofollow)
  "Resolve PATH within ROOT, following links component by component.
Absolute link targets restart at ROOT; .. cannot escape it.
With NOFOLLOW, leave the final component unresolved."
  ;; Resolution and the later access are separate steps, so the
  ;; environment is trusted not to swap a component for a link between
  ;; them; a user-owned namespace already runs with the user's rights.
  (let ((todo (split-string path "/" t)) done (links 0))
    (while todo
      (let ((part (pop todo)))
        (cond
         ((equal part "."))
         ;; Names arrive expanded, so .. comes from a link target, where
         ;; the kernel steps back only over an existing directory.  Keep
         ;; anything else, so that missing/../x fails on the host too.
         ((equal part "..")
          (when (or (null done) (file-directory-p (tramp-ns--join root done)))
            (pop done)))
         (t
          (let* ((candidate (tramp-ns--join root (cons part done)))
                 (target (unless (and nofollow (null todo))
                           (file-symlink-p candidate))))
            (if (not target) (push part done)
              (when (> (cl-incf links) 40)
                (signal 'tramp-ns-link-loop (list path)))
              (when (string-prefix-p "/" target) (setq done nil))
              (setq todo (append (split-string target "/" t) todo))))))))
    (concat (tramp-ns--join root done)
            (when (and done (not nofollow) (string-suffix-p "/" path)) "/"))))

(defun tramp-ns--local (file &optional nofollow)
  "Translate an ns FILE to a host path, optionally with NOFOLLOW."
  (if (not (tramp-ns--file-name-p file)) file
    (let* ((vec (tramp-dissect-file-name file))
           (p (tramp-ns--connection vec)))
      (tramp-ns--resolve
       (format "/proc/%d/root" (process-get p 'tramp-ns-pid))
       (file-name-unquote (tramp-file-name-localname vec)) nofollow))))

(defun tramp-ns--target (file newname)
  "Return NEWNAME, or FILE's name inside it if NEWNAME is a directory name."
  (if (directory-name-p newname)
      (expand-file-name (file-name-nondirectory (directory-file-name file)) newname)
    newname))

(defun tramp-ns--flush (file)
  "Drop TRAMP's cached properties of FILE and below it, if FILE is an ns name."
  (when (tramp-ns--file-name-p file)
    (let ((vec (tramp-dissect-file-name file)))
      (tramp-flush-directory-properties vec (tramp-file-name-localname vec)))))

;;;; File operations

(defun tramp-ns--file-operation (operation file &rest args)
  "Run file OPERATION on FILE and ARGS with ns names translated."
  (let ((nofollow (or (memq operation '(add-name-to-file delete-directory delete-file
                                        file-attributes make-directory rename-file))
                      (memq 'nofollow args)))
        ;; Name the target here, since the host would add FILE's name to a
        ;; directory NEWNAME and follow a link it finds there.  Copying
        ;; writes through a link at the target; renaming and linking
        ;; replace it.
        (newname (and (memq operation '(add-name-to-file copy-file rename-file))
                      (tramp-ns--target file (car args)))))
    (unwind-protect
        (cond
         (newname
          (apply operation (tramp-ns--local file nofollow)
                 (tramp-ns--local newname nofollow) (cdr args)))
         ((memq operation '(file-directory-p file-executable-p file-exists-p
                            file-readable-p file-regular-p file-writable-p))
          ;; Like the host's predicates, answer nil for a link loop.
          (ignore-error tramp-ns-link-loop
            (apply operation (tramp-ns--local file nofollow) args)))
         (t (apply operation (tramp-ns--local file nofollow) args)))
      (when (memq operation '(add-name-to-file copy-file delete-directory delete-file
                              make-directory rename-file set-file-acl set-file-modes
                              set-file-selinux-context set-file-times))
        (tramp-ns--flush file)
        (when newname (tramp-ns--flush newname))))))

(defun tramp-ns--completions (file directory)
  "Return completions for FILE in DIRECTORY using TRAMP's filtering."
  (tramp-skeleton-file-name-all-completions file directory
    (directory-files (tramp-ns--local directory))))

(defun tramp-ns--make-symbolic-link (target linkname &optional ok-if-already-exists)
  "Make LINKNAME a link to TARGET inside the environment.
OK-IF-ALREADY-EXISTS is as for `make-symbolic-link'."
  (tramp-skeleton-make-symbolic-link target linkname ok-if-already-exists
    (make-symbolic-link target (tramp-ns--local linkname t) ok-if-already-exists)))

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
      (write-region start end (tramp-ns--local filename) append 'nomessage
                    nil (and (eq mustbenew 'excl) 'excl)))
    (setq coding-system-used last-coding-system-used)))

;;;; Processes

(defun tramp-ns--exec-path ()
  "Return the inner process's executable search path."
  (let* ((vec (tramp-dissect-file-name (expand-file-name default-directory)))
         (env (tramp-ns--environment (tramp-ns--connection vec))))
    ;; Like `tramp-sh-handle-exec-path', end with the working directory.
    (append (split-string (or (getenv-internal "PATH" env) "") ":")
            (list (tramp-file-name-localname vec)))))

(defun tramp-ns--process-path (vec file)
  "Return FILE, named for a process in VEC's environment, as a host path."
  (let ((file (expand-file-name file)))
    (when (and (file-remote-p file) (not (tramp-ns--file-name-p file)))
      (tramp-user-error vec "Cannot pass %s to a process in %s"
                        file (tramp-file-name-host vec)))
    (tramp-ns--local file)))

(defun tramp-ns--destination (vec destination)
  "Translate the file names in a `call-process' DESTINATION for VEC."
  (pcase destination
    (`(:file ,file) `(:file ,(tramp-ns--process-path vec file)))
    (`(,stdout ,stderr)
     (list (tramp-ns--destination vec stdout)
           (if (stringp stderr) (tramp-ns--process-path vec stderr) stderr)))
    (_ destination)))

(defun tramp-ns--process-file (program &optional infile destination display &rest args)
  "Run PROGRAM with INFILE, DESTINATION, DISPLAY and ARGS inside the environment."
  (let* ((vec (tramp-dissect-file-name (expand-file-name default-directory)))
         (p (tramp-ns--connection vec))
         (infile (and infile (tramp-ns--process-path vec infile)))
         (destination (tramp-ns--destination vec destination))
         (nsenter (or (executable-find "nsenter")
                      (tramp-user-error vec "Cannot find nsenter")))
         ;; As `tramp-handle-make-process' does for asynchronous ones.
         (process-environment (append (list (concat "INSIDE_EMACS=" (tramp-inside-emacs)))
                                      tramp-remote-process-environment
                                      (tramp-ns--environment p)))
         (default-directory "/"))
    (unwind-protect
        (apply #'call-process nsenter infile destination display
               (concat "--wdns=" (tramp-file-name-localname vec))
               (append (tramp-ns--login-args (process-get p 'tramp-ns-pid))
                       (list program) args))
      ;; As in `tramp-sh-handle-process-file'.
      (when process-file-side-effects
        (tramp-flush-directory-properties vec "/")))))

(defun tramp-ns--make-process (&rest args)
  "Like `tramp-handle-make-process' for ARGS, with the inner environment."
  (let* ((vec (tramp-dissect-file-name (expand-file-name default-directory)))
         (process-environment (tramp-ns--environment (tramp-ns--connection vec))))
    (apply #'tramp-handle-make-process args)))

;;;; Registration

(defconst tramp-ns-file-name-handler-alist
  '((abbreviate-file-name . tramp-handle-abbreviate-file-name)
    (access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-ns--file-operation)
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-ns--file-operation)
    (delete-directory . tramp-ns--file-operation)
    (delete-file . tramp-ns--file-operation)
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes . tramp-handle-directory-files-and-attributes)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-ns--exec-path)
    ;; Unlike the generic handler, this one reads a relative local name,
    ;; as in /ns:box:file, relative to ~, as ssh does.
    (expand-file-name . tramp-sh-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . tramp-ns--file-operation)
    (file-attributes . tramp-ns--file-operation)
    (file-directory-p . tramp-ns--file-operation)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-ns--file-operation)
    (file-exists-p . tramp-ns--file-operation)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-ns--completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . tramp-ns--file-operation)
    (file-readable-p . tramp-ns--file-operation)
    (file-regular-p . tramp-ns--file-operation)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-ns--file-operation)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . tramp-ns--file-operation)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-ns--insert)
    (list-system-processes . tramp-handle-list-system-processes)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-ns--file-operation)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-ns--make-process)
    (make-symbolic-link . tramp-ns--make-symbolic-link)
    (memory-info . tramp-handle-memory-info)
    (process-attributes . tramp-handle-process-attributes)
    (process-file . tramp-ns--process-file)
    (rename-file . tramp-ns--file-operation)
    (set-file-acl . tramp-ns--file-operation)
    (set-file-modes . tramp-ns--file-operation)
    (set-file-selinux-context . tramp-ns--file-operation)
    (set-file-times . tramp-ns--file-operation)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . tramp-ns--home)
    (tramp-get-remote-gid . ignore)
    (tramp-get-remote-groups . ignore)
    (tramp-get-remote-uid . ignore)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-ns--write))
  "Operations handled by the ns backend.")

(defun tramp-ns--host-root (name)
  "Return NAME's host root and its ns name, if NAME's connection is live."
  (when-let* (((tramp-ns--file-name-p name))
              (vec (tramp-dissect-file-name name))
              (p (tramp-get-connection-process vec))
              (pid (process-get p 'tramp-ns-pid)))
    (cons (format "/proc/%d/root/" pid) (tramp-make-tramp-file-name vec "/"))))

(defun tramp-ns--error-names (names data)
  "Return error DATA with host roots of NAMES' connections shown as ns names."
  ;; TRAMP's `signal-hook-function' may already have turned DATA into
  ;; one message, so replace within strings rather than whole strings.
  (let ((roots (delq nil (mapcar #'tramp-ns--host-root (seq-filter #'stringp names)))))
    (mapcar (lambda (datum)
              (if (not (stringp datum)) datum
                (seq-reduce (pcase-lambda (string `(,root . ,ns))
                              (string-replace root ns string))
                            roots datum)))
            data)))

(defun tramp-ns-file-name-handler (operation &rest args)
  "Dispatch OPERATION with ARGS through TRAMP's ns backend."
  (let ((handler (alist-get operation tramp-ns-file-name-handler-alist)))
    (save-match-data
      (condition-case err
          (cond ((eq handler #'tramp-ns--file-operation)
                 (apply handler operation args))
                (handler (apply handler args))
                (t (tramp-run-real-handler operation args)))
        (file-error
         (signal (car err)
                 (tramp-ns--error-names (cons default-directory args) (cdr err))))))))

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

(defun tramp-ns-unload-function ()
  "Remove the ns backend from TRAMP when `unload-feature' unloads it."
  (dolist (vec (tramp-list-connections))
    (when (tramp-ns--file-name-p vec)
      (tramp-cleanup-connection vec)))
  (setq tramp-foreign-file-name-handler-alist
        (assq-delete-all #'tramp-ns--file-name-p tramp-foreign-file-name-handler-alist))
  (setq tramp-methods (assoc-delete-all "ns" tramp-methods))
  (setq tramp-completion-function-alist
        (assoc-delete-all "ns" tramp-completion-function-alist))
  ;; Let `unload-feature' continue with its usual cleanup.
  nil)

(provide 'tramp-ns)

;;;###autoload
(with-eval-after-load 'tramp
  (require 'tramp-ns))
;;; tramp-ns.el ends here

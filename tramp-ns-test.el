;;; tramp-ns-test.el --- Tests for tramp-ns in real environments -*- lexical-binding: t; -*-

;;; Commentary:

;; NSENTER_TEST_ENTER is the command that enters the environment, split on
;; spaces, like "podman exec -i box"; the justfile sets it.  Without it,
;; only the tests that need no environment run.

;;; Code:

(require 'ert)
(require 'tramp-ns)
(require 'dired)
(require 'compile)
(require 'vc-git)
(require 'shell)
(require 'eshell)

(defvar tramp-ns-test-environments
  (when-let* ((enter (getenv "NSENTER_TEST_ENTER")))
    `(("test" :enter ,(split-string-and-unquote enter))))
  "The environment the tests enter, or nil to skip the tests that need one.")
(defmacro tramp-ns-test-with-directory (&rest body)
  "Evaluate BODY in a new directory inside the environment, deleted afterwards."
  (declare (indent 0))
  (let ((dir (make-symbol "dir")))
    `(let ((tramp-ns-environments tramp-ns-test-environments))
       (skip-unless tramp-ns-environments)
       (let* ((,dir (let ((default-directory "/ns:test:/tmp/"))
                      (file-name-as-directory
                       (make-nearby-temp-file "tramp-ns-test-" t))))
              (default-directory ,dir))
         (unwind-protect (progn ,@body)
           (dolist (b (buffer-list))
             (when (with-current-buffer b
                     (and buffer-file-name (string-prefix-p ,dir buffer-file-name)))
               (with-current-buffer b (set-buffer-modified-p nil))
               (kill-buffer b)))
           (unwind-protect (delete-directory ,dir t)
             (tramp-cleanup-connection (tramp-dissect-file-name ,dir))))))))

(defun tramp-ns-test-output (&rest args)
  "Return the output of running ARGS inside, which must succeed."
  (with-temp-buffer
    (should (zerop (apply #'process-file (car args) nil t nil (cdr args))))
    (buffer-string)))

(defun tramp-ns-test-wait (p)
  "Wait for process P to exit successfully, killing it if it does not."
  (let ((deadline (+ (float-time) 20)))
    (unwind-protect
        (progn
          (while (and (process-live-p p) (< (float-time) deadline))
            (accept-process-output p 0.05))
          (should-not (process-live-p p))
          (should (zerop (process-exit-status p))))
      (when (process-live-p p) (delete-process p)))))

(ert-deftest tramp-ns-edit-and-files ()
  (tramp-ns-test-with-directory
    (let* ((file (expand-file-name "notes.txt"))
           (b (find-file-noselect file)))
      (with-current-buffer b
        (insert "first\n") (save-buffer)
        (should (equal buffer-file-name file))
        (should (verify-visited-file-modtime b))
        (goto-char (point-max)) (insert "second\n") (save-buffer))
      (should (file-exists-p (concat file "~")))
      (should (file-exists-p file))
      (should (file-regular-p file))
      (should (file-readable-p file))
      (should (file-writable-p file))
      (should-error (write-region "new" nil file nil nil nil 'excl)
                    :type 'file-already-exists)
      (should (equal (tramp-ns-test-output "cat" "notes.txt") "first\nsecond\n"))
      (copy-file file (expand-file-name "copy.txt"))
      (make-directory (expand-file-name "into"))
      (copy-file file (expand-file-name "into/"))
      (should (file-exists-p (expand-file-name "into/notes.txt")))
      (rename-file (expand-file-name "copy.txt") (expand-file-name "renamed.txt"))
      (should (member "renamed.txt" (directory-files default-directory)))
      (should (member "notes.txt" (file-name-all-completions "not" default-directory)))
      (should (member file (directory-files default-directory t "notes")))
      (let ((local (file-local-copy file)))
        (unwind-protect
            (should (equal (tramp-ns--read local) "first\nsecond\n"))
          (delete-file local)))
      (let ((err (should-error (insert-file-contents (expand-file-name "missing"))
                               :type 'file-missing)))
        (should (string-search (expand-file-name "missing") (error-message-string err)))
        (should-not (string-search "/proc/" (error-message-string err))))
      (delete-file (expand-file-name "renamed.txt"))
      (should-not (file-exists-p (expand-file-name "renamed.txt"))))))

(ert-deftest tramp-ns-links-stay-in-root ()
  (tramp-ns-test-with-directory
    (tramp-ns-test-output
     "sh" "-c"
     "mkdir dir; printf inner > dir/data; ln -s \"$PWD/dir\" absolute; ln -s dir relative; ln -s /../../etc/os-release rooted; ln -s missing dangling; ln -s loop loop; ln -s \"$PWD/dir/data\" datalink; ln -s \"$PWD/dir/data\" dir/source")
    (dolist (name '("absolute/data" "relative/data"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name name))
        (should (equal (buffer-string) "inner"))))
    (should (file-symlink-p (expand-file-name "absolute")))
    (should (stringp (file-attribute-type
                      (file-attributes (expand-file-name "absolute/")))))
    (should (file-directory-p (expand-file-name "absolute")))
    (should (member "absolute/" (file-name-all-completions "abs" default-directory)))
    (should-not (file-exists-p (expand-file-name "dangling")))
    (dolist (predicate '(file-exists-p file-readable-p file-directory-p file-regular-p))
      (should-not (funcall predicate (expand-file-name "loop/x"))))
    (should-error (file-attributes (expand-file-name "loop/x")) :type 'file-error)
    (write-region "copied" nil (expand-file-name "source") nil 'silent)
    (copy-file (expand-file-name "source") (expand-file-name "datalink") t)
    (should (equal (tramp-ns-test-output "cat" "dir/data") "copied"))
    (write-region "through" nil (expand-file-name "source") nil 'silent)
    (copy-file (expand-file-name "source") (expand-file-name "dir/") t)
    (should (equal (tramp-ns-test-output "cat" "dir/data") "through"))
    (add-name-to-file (expand-file-name "relative") (expand-file-name "hardlink"))
    (should (equal (file-symlink-p (expand-file-name "hardlink")) "dir"))
    (make-symbolic-link "dir" (expand-file-name "made"))
    (make-symbolic-link (expand-file-name "dir") (expand-file-name "made-ns"))
    (should (equal (file-symlink-p (expand-file-name "made-ns"))
                   (file-local-name (expand-file-name "dir"))))
    (should (equal (file-symlink-p (expand-file-name "made")) "dir"))
    (should (file-exists-p (expand-file-name "made/data")))
    (dolist (name '("plain" "second"))
      (write-region "plain" nil (expand-file-name name) nil 'silent)
      (file-truename (expand-file-name name)))
    (rename-file (expand-file-name "made") (expand-file-name "plain") t)
    (should (equal (file-truename (expand-file-name "plain")) (expand-file-name "dir")))
    (tramp-ns-test-output "ln" "-sf" "dir" "second")
    (should (equal (file-truename (expand-file-name "second")) (expand-file-name "dir")))
    (delete-file (expand-file-name "absolute"))
    (should (file-exists-p (expand-file-name "dir/data")))
    ;; PATH is deliberately not normalized by expand-file-name first.
    (should (equal (tramp-ns--read (tramp-ns--local (concat default-directory "rooted")))
                   (tramp-ns-test-output "cat" "/etc/os-release")))))

(ert-deftest tramp-ns-processes ()
  (tramp-ns-test-with-directory
    (should (equal (string-trim (tramp-ns-test-output "pwd"))
                   (directory-file-name (file-local-name default-directory))))
    (let ((process-environment (cons "NSENTER_TEST_BOUND=from-emacs" process-environment)))
      (should (equal (tramp-ns-test-output "sh" "-c" "printf %s \"$NSENTER_TEST_BOUND\"")
                     "from-emacs")))
    (write-region "input" nil (expand-file-name "in.txt") nil 'silent)
    (with-temp-buffer
      (should (zerop (process-file "cat" "in.txt" t)))
      (should (equal (buffer-string) "input")))
    (should (zerop (process-file "sh" nil '(nil "err.txt") nil "-c" "printf oops >&2")))
    (should (equal (tramp-ns-test-output "cat" "err.txt") "oops"))
    (should (zerop (process-file "printf" nil '(:file "out.txt") nil "out")))
    (should (equal (tramp-ns-test-output "cat" "out.txt") "out"))
    (should (zerop (process-file "sh" nil '((:file "both.txt") "both-err.txt") nil
                                 "-c" "printf out; printf err >&2")))
    (should (equal (tramp-ns-test-output "cat" "both.txt") "out"))
    (should (equal (tramp-ns-test-output "cat" "both-err.txt") "err"))
    (should-error (process-file "cat" "/ssh:elsewhere:/x") :type 'user-error)
    (should (string-match-p ",tramp:" (tramp-ns-test-output "sh" "-c" "printf %s \"$INSIDE_EMACS\"")))
    (should (equal (tramp-ns-test-output "sh" "-c" "printf %s \"$PAGER\"") "cat"))
    (with-temp-buffer
      (let ((process-connection-type nil)
            (p (start-file-process "tramp-ns-async" (current-buffer)
                                   "sh" "-c" "printf \"async $INSIDE_EMACS\"")))
        (tramp-ns-test-wait p)
        (should (string-match-p "\\`async .*,tramp:" (buffer-string)))))
    (with-temp-buffer
      (should (zerop (shell-command "printf shell" (current-buffer))))
      (should (equal (buffer-string) "shell")))
    (let* ((compilation-read-command nil)
           (b (compilation-start "printf 'source.c:1: test\\n'"))
           (p (get-buffer-process b)))
      (unwind-protect
          (progn (tramp-ns-test-wait p)
                 (with-current-buffer b
                   (should (string-match-p "source.c:1: test" (buffer-string)))))
        (kill-buffer b)))))

(ert-deftest tramp-ns-shell-and-eshell ()
  (tramp-ns-test-with-directory
    (save-window-excursion
      (let* ((explicit-shell-file-name "/bin/sh")
             (b (shell (generate-new-buffer "*tramp-ns-shell*")))
             (p (get-buffer-process b)))
        (unwind-protect
            (progn
              (process-send-string p "printf 'tramp-ns-shell-%s\\n' \"$PWD\"; exit\n")
              (tramp-ns-test-wait p)
              (with-current-buffer b
                (should (string-search (concat "tramp-ns-shell-"
                                               (file-local-name
                                                (directory-file-name default-directory)))
                                       (buffer-string)))))
          (when (process-live-p p) (delete-process p))
          (kill-buffer b))))
    (with-temp-buffer
      (eshell-command "printf tramp-ns-eshell-ok" t)
      (should (string-match-p "tramp-ns-eshell-ok" (buffer-string))))))

(ert-deftest tramp-ns-dired-and-git ()
  (tramp-ns-test-with-directory
    (skip-unless (executable-find "git" t))
    (tramp-ns-test-output "git" "init" "-q")
    (write-region "tracked\n" nil (expand-file-name "tracked") nil 'silent)
    (tramp-ns-test-output "git" "add" "tracked")
    (tramp-ns-test-output "git" "-c" "user.name=Test" "-c" "user.email=test@example.org"
                         "commit" "-qm" "Initial")
    (let ((file (expand-file-name "tracked")))
      (should (vc-git-registered file))
      (should (eq (vc-backend file) 'Git))
      (should (eq (vc-state file) 'up-to-date))
      (should-not (vc-backend (expand-file-name "untracked"))))
    (let ((b (find-file-noselect (expand-file-name "tracked"))))
      (unwind-protect
          (with-current-buffer b (should vc-mode))
        (kill-buffer b)))
    (save-window-excursion
      (let ((b (dired-noselect default-directory)))
        (unwind-protect
            (with-current-buffer b
              (should (dired-goto-file (expand-file-name "tracked")))
              (revert-buffer))
          (kill-buffer b))))))

(ert-deftest tramp-ns-resolve-stays-in-root ()
  (let ((root (make-temp-file "tramp-ns-root" t)))
    (unwind-protect
        (progn
          (make-directory (concat root "/dir"))
          (make-symbolic-link "/dir" (concat root "/absolute"))
          (make-symbolic-link "../../.." (concat root "/dir/up"))
          (make-symbolic-link "loop" (concat root "/loop"))
          (should (equal (tramp-ns--resolve root "/absolute/x") (concat root "/dir/x")))
          (should (equal (tramp-ns--resolve root "/absolute/") (concat root "/dir/")))
          (should (equal (tramp-ns--resolve root "/absolute" t) (concat root "/absolute")))
          (should (equal (tramp-ns--resolve root "/dir/up/etc") (concat root "/etc")))
          (should (equal (tramp-ns--resolve root "/../../dir/./x") (concat root "/dir/x")))
          (should (equal (tramp-ns--resolve root "/dir/../dir/x") (concat root "/dir/x")))
          (make-symbolic-link "missing/../dir" (concat root "/through-missing"))
          (should-not (file-exists-p (tramp-ns--resolve root "/through-missing")))
          (should-error (tramp-ns--resolve root "/loop") :type 'tramp-ns-link-loop))
      (delete-directory root t))))

(ert-deftest tramp-ns-host-files ()
  ;; An environment that enters nothing has the host's root, so file
  ;; operations run without a container.
  (let* ((tramp-ns-environments '(("host" :enter ("sh" "-c" "exec \"$@\"" "host"))))
         (local (file-name-as-directory (make-temp-file "tramp-ns-host" t)))
         (dir (concat "/ns:host:" local))
         (vec (tramp-dissect-file-name dir)))
    (unwind-protect
        (let ((file (concat dir "file")))
          (write-region "data" nil file nil 'silent)
          (should (equal (tramp-ns--read (concat local "file")) "data"))
          (make-directory (concat dir "into"))
          (copy-file file (concat dir "into/"))
          (should (file-regular-p (concat dir "into/file")))
          (rename-file (concat dir "into/file") (concat dir "moved"))
          (should (equal (directory-files dir nil "\\`[^.]") '("file" "into" "moved")))
          (make-symbolic-link "loop" (concat dir "loop"))
          (should-not (file-exists-p (concat dir "loop/x")))
          (should (equal (file-name-all-completions "in" dir) '("into/")))
          (let ((tramp-fnac-add-trailing-slash nil))
            (should (equal (file-name-all-completions "in" dir) '("into"))))
          (make-directory (concat dir "inside"))
          (should (equal (sort (file-name-all-completions "in" dir) #'string<)
                         '("inside/" "into/")))
          (let ((err (should-error (insert-file-contents (concat dir "missing")))))
            (should (string-search (concat dir "missing") (error-message-string err))))
          (should (equal (file-acl file) (file-acl (concat local "file"))))
          (should (eq (set-file-acl file (file-acl file))
                      (set-file-acl (concat local "file") (file-acl file))))
          (delete-file (concat dir "moved"))
          (should-not (file-exists-p (concat local "moved"))))
      (delete-directory local t)
      (tramp-cleanup-connection vec))))

(ert-deftest tramp-ns-unload ()
  (unwind-protect
      (progn
        (unload-feature 'tramp-ns t)
        (should-not (assoc "ns" tramp-methods))
        (should-not (assoc "ns" tramp-completion-function-alist))
        (should-not (rassq 'tramp-ns-file-name-handler
                           tramp-foreign-file-name-handler-alist))
        (should-not (seq-some (lambda (vec) (equal (tramp-file-name-method vec) "ns"))
                              (tramp-list-connections))))
    (require 'tramp-ns)))

(ert-deftest tramp-ns-connection-lifetime ()
  (skip-unless tramp-ns-test-environments)
  (let* ((tramp-ns-environments tramp-ns-test-environments)
         (vec (tramp-dissect-file-name "/ns:test:/")))
    (unwind-protect
        (let* ((p (tramp-ns--connection vec))
               (pid (process-get p 'tramp-ns-pid))
               (deadline (+ (float-time) 10)))
          (process-send-eof p)
          (while (and (process-live-p p) (< (float-time) deadline))
            (accept-process-output p 0.05))
          (should-not (file-exists-p (format "/proc/%d" pid)))
          (should (file-exists-p "/ns:test:/etc/os-release"))
          (should-not (= pid (process-get (tramp-ns--connection vec) 'tramp-ns-pid)))
          (let* ((old (tramp-ns--connection vec))
                 (prefix (plist-get (cdar tramp-ns-environments) :enter))
                 (tramp-ns-environments `(("test" :enter ("env" ,@prefix)))))
            (should-not (eq old (tramp-ns--connection vec)))
            (should-not (process-live-p old))))
      (tramp-cleanup-connection vec))))

(ert-deftest tramp-ns-stale-connection ()
  (skip-unless tramp-ns-test-environments)
  (let* ((prefix (plist-get (cdar tramp-ns-test-environments) :enter))
         (tramp-ns-environments
          (list (list "wrapped" :enter (append '("sh" "-c" "\"$@\"; sleep 60" "wrap")
                                               prefix))))
         (vec (tramp-dissect-file-name "/ns:wrapped:/")))
    (unwind-protect
        (let* ((p (tramp-ns--connection vec))
               (pid (process-get p 'tramp-ns-pid))
               (deadline (+ (float-time) 10)))
          (signal-process pid 'kill)
          (while (and (tramp-ns--live-p p) (< (float-time) deadline))
            (sleep-for 0.05))
          (should (process-live-p p))
          (should (file-exists-p "/ns:wrapped:/etc/os-release"))
          (should-not (= pid (process-get (tramp-ns--connection vec) 'tramp-ns-pid))))
      (tramp-cleanup-connection vec))))

(ert-deftest tramp-ns-startup-timeouts ()
  (let* ((tramp-ns-start-timeout 0.05)
         (tramp-ns-environments
          (list (list "slow" :enter '("sh" "-c" "sleep 2; exec \"$@\"" "test"))))
         (vec (tramp-dissect-file-name "/ns:slow:/")))
    (unwind-protect
        (progn
          (let ((non-essential t))
            (should-not (file-exists-p "/ns:slow:/etc/os-release")))
          (should-not (tramp-get-connection-process vec))
          (dolist (timeout '(nil 0.15))
            (setf (plist-get (cdar tramp-ns-environments) :timeout) timeout)
            (let ((start (float-time)))
              (should-error (file-exists-p "/ns:slow:/etc/os-release"))
              (let ((elapsed (- (float-time) start))
                    (expected (or timeout tramp-ns-start-timeout)))
                (should (>= elapsed expected))
                (should (< elapsed (+ expected 1)))))
            (should-not (process-live-p (tramp-get-connection-process vec)))))
      (tramp-cleanup-connection vec))))

(ert-deftest tramp-ns-bad-environments ()
  (let ((tramp-ns-environments '(("noenter") ("badenter" :enter "sh")
                                 ("badtimeout" :enter ("sh") :timeout "1")
                                 ("good" :enter ("sh" "-c" "exec \"$@\"" "good")))))
    (dolist (name '("nowhere" "noenter" "badenter" "badtimeout" "user@good" "good#22"))
      (should-error (file-exists-p (format "/ns:%s:/" name)) :type 'user-error))
    (let ((tramp-ns-start-timeout -1))
      (should-error (file-exists-p "/ns:good:/") :type 'user-error))))

(ert-deftest tramp-ns-home-paths ()
  (tramp-ns-test-with-directory
    (write-region "home data" nil (expand-file-name "relative.txt") nil 'silent)
    (let* ((home (file-local-name default-directory))
           (prefix (plist-get (cdar tramp-ns-environments) :enter))
           (tramp-ns-environments
            (cons (list "home" :enter (append prefix (list "env" (concat "HOME=" home))))
                  tramp-ns-environments))
           (vec (tramp-dissect-file-name "/ns:home:/")))
      (unwind-protect
          (progn
            (dolist (name '("/ns:home:" "/ns:home:~" "/ns:home:~/"))
              (should (equal (file-name-as-directory (expand-file-name name))
                             (concat "/ns:home:" home)))
              (should (file-directory-p name))
              (should (member "relative.txt" (directory-files name))))
            (should (member "relative.txt" (file-name-all-completions "rel" "/ns:home:")))
            (should (equal (tramp-ns--read "/ns:home:relative.txt") "home data"))
            (should (equal (tramp-ns--read "/ns:home:~/relative.txt") "home data"))
            (let* ((entry (tramp-ns-test-output "getent" "passwd" "root"))
                   (home (nth 5 (split-string (string-trim-right entry) ":"))))
              (should (equal (expand-file-name "/ns:home:~root/x")
                             (concat "/ns:home:" home "/x"))))
            (let ((buffer (find-file-noselect "/ns:home:")))
              (unwind-protect
                  (with-current-buffer buffer
                    (should (eq major-mode 'dired-mode))
                    (should (equal (string-trim (tramp-ns-test-output "pwd"))
                                   (directory-file-name home))))
                (kill-buffer buffer))))
        (tramp-cleanup-connection vec)))))

(provide 'tramp-ns-test)
;;; tramp-ns-test.el ends here

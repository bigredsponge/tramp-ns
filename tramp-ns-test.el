;;; tramp-ns-test.el --- Tests for tramp-ns in real environments -*- lexical-binding: t; -*-

;;; Commentary:

;; NSENTER_TEST_ENTER is the command that enters the environment, split on
;; spaces, like "podman exec -i box"; the justfile sets it.

;;; Code:

(require 'ert)
(require 'tramp-ns)
(require 'dired)
(require 'compile)
(require 'vc-git)
(require 'shell)
(require 'eshell)

(setq tramp-ns-environments
      `(("test" :enter ,(split-string (getenv "NSENTER_TEST_ENTER")))))
(defvar tramp-ns-test-root
  (format "/ns:test:/tmp/tramp-ns-test-%d/" (emacs-pid))
  "The directory inside that a test makes and then deletes.")

(defmacro tramp-ns-test-with-directory (&rest body)
  "Evaluate BODY in a fresh `tramp-ns-test-root', deleted afterwards."
  (declare (indent 0))
  `(let ((default-directory tramp-ns-test-root))
     (make-directory default-directory t)
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (with-current-buffer b
                 (and buffer-file-name
                      (string-prefix-p tramp-ns-test-root buffer-file-name)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory tramp-ns-test-root t))))

(defun tramp-ns-test-output (&rest args)
  "Return the output of running ARGS inside, which must succeed."
  (with-temp-buffer
    (should (zerop (apply #'process-file (car args) nil t nil (cdr args))))
    (buffer-string)))

(defun tramp-ns-test-wait (p)
  "Wait for process P to exit successfully."
  (let ((deadline (+ (float-time) 20)))
    (while (and (process-live-p p) (< (float-time) deadline))
      (accept-process-output p 0.05))
    (should-not (process-live-p p))
    (should (zerop (process-exit-status p)))))

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
      (should (equal (tramp-ns-test-output "cat" "notes.txt") "first\nsecond\n"))
      (copy-file file (expand-file-name "copy.txt"))
      (rename-file (expand-file-name "copy.txt") (expand-file-name "renamed.txt"))
      (should (member "renamed.txt" (directory-files default-directory)))
      (should (member "notes.txt" (file-name-all-completions "not" default-directory)))
      (should (member file (directory-files default-directory t "notes")))
      (let ((local (file-local-copy file)))
        (unwind-protect
            (should (equal (tramp-ns--read local) "first\nsecond\n"))
          (delete-file local)))
      (delete-file (expand-file-name "renamed.txt"))
      (should-not (file-exists-p (expand-file-name "renamed.txt"))))))

(ert-deftest tramp-ns-links-stay-in-root ()
  (tramp-ns-test-with-directory
    (tramp-ns-test-output
     "sh" "-c"
     "mkdir dir; printf inner > dir/data; ln -s \"$PWD/dir\" absolute; ln -s dir relative; ln -s /../../etc/os-release rooted; ln -s missing dangling")
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
              (process-send-string p "printf 'tramp-ns-shell-ok\\n'; exit\n")
              (tramp-ns-test-wait p)
              (with-current-buffer b
                (should (string-match-p "tramp-ns-shell-ok" (buffer-string)))))
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
      (should (eq (vc-git-state file) 'up-to-date)))
    (save-window-excursion
      (let ((b (dired-noselect default-directory)))
        (unwind-protect
            (with-current-buffer b
              (should (dired-goto-file (expand-file-name "tracked")))
              (revert-buffer))
          (kill-buffer b))))))

(ert-deftest tramp-ns-connection-lifetime ()
  (let* ((vec (tramp-dissect-file-name "/ns:test:/"))
         (p (tramp-ns--connection vec))
         (pid (process-get p 'tramp-ns-pid)))
    (process-send-eof p)
    (let ((deadline (+ (float-time) 10)))
      (while (and (process-live-p p) (< (float-time) deadline))
        (accept-process-output p 0.05)))
    (should-not (file-exists-p (format "/proc/%d" pid)))
    (should (file-exists-p "/ns:test:/etc/os-release"))
    (should-not (= pid (process-get (tramp-ns--connection vec) 'tramp-ns-pid)))))

(ert-deftest tramp-ns-startup-timeouts ()
  (let* ((tramp-ns-start-timeout 0.05)
         (tramp-ns-environments
          '(("slow" :enter ("sh" "-c" "sleep 2; exec \"$@\"" "test"))))
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

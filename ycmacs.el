(require 'json)
(require 'popup)
(require 'request)

(random t)


(defgroup ycmacs nil
  "Emacs YCM interface")

(defcustom ycm/path-to-python2 "/usr/bin/python2"
  "Path to python2 interpreter"
  :type '(string) :group 'ycmacs)

(defcustom ycm/path-to-ycmd (expand-file-name "~/repos/ycmd/ycmd")
  "Path to the ycmd server (location of __main__.py)"
  :type '(string) :group 'ycmacs)

(defcustom ycm/server-idle-suicide-seconds 10800
  "How long to wait idle until the server dies"
  :type '(integer) :group 'ycmacs)

(defcustom ycm/hmac-secret-length 16
  "Length of HMAC secret"
  :type '(integer) :group 'ycmacs)

(defcustom ycm/idle-delay 0.5
  "How long to wait idle before starting completion."
  :type '(number) :group 'ycmacs)

(defvar ycm/hmac-secret nil)
(defvar ycm/ycmd-process nil)
(defvar ycm/ycmacs-buffer nil)
(defvar ycm/ycmd-port nil)
(defvar ycm/timer nil)

(defvar-local ycm/popup nil)
(defvar-local ycm/track-buffer nil)
(defvar-local ycm/track-window nil)
(defvar-local ycm/track-tick nil)
(defvar-local ycm/track-point nil)


(defun ycm/log (str)
  (message str))

(defun ycm/log-line (str) (ycm/log str))


(defun ycm/ycmd-filter (proc str)
  (catch 'ready
    (when (string-match "\\`serving on http://\\([0-9]+\\.\\)\\{3\\}[0-9]+:[0-9]+" str)
      (setq ycm/ycmd-port (string-to-number (car (last (split-string str ":")))))
      (throw 'ready ""))
    (let ((lines (split-string str "\n")))
      (while lines
        (when (not (string= "" (car lines)))
          (ycm/log-line (format "--- %s" (car lines))))
        (setq lines (cdr lines))))))


(defun ycm/get-default-settings ()
  "Loads the YCM default settings file and parses it."
  (let ((fn (expand-file-name "default_settings.json" ycm/path-to-ycmd))
        (json-object-type 'hash-table))
    (with-temp-buffer
      (insert-file-contents fn)
      (json-read-from-string (buffer-string)))))


(defun ycm/generate-hmac-secret ()
  "Generates an HMAC secret of length ycm/hmac-secret-length."
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%&^*()[]{},./<>?")
        (n-chars 82))
    (with-temp-buffer
      (dotimes (i ycm/hmac-secret-length)
        (insert (elt chars (random n-chars))))
      (setq ycm/hmac-secret (string-to-unibyte (buffer-string))))))


;; Thanks to Derek Upham for his implementation of HMAC-SHA1.
(defun ycm/hmac-sha256 (key msg)
  "Computes the hex representation of the HMAC SHA256 algorithm given key and message."
  (when (multibyte-string-p key) (error "ycm/hmac-sha256: Key must be unibyte"))
  (when (multibyte-string-p msg) (error "ycm/hmac-sha256: Msg must be unibyte"))
  (let* ((block-size 64)
         (outer-pad (make-vector block-size #x5c))
         (inner-pad (make-vector block-size #x36))
         (key-block (make-vector block-size 0)))
    (when (< block-size (length key))
      (setq key (secure-hash 'sha256 nil nil t)))
    (dotimes (i (length key))
      (aset key-block i (aref key i)))
    (dotimes (i block-size)
      (aset inner-pad i (logxor (aref inner-pad i) (aref key-block i)))
      (aset outer-pad i (logxor (aref outer-pad i) (aref key-block i))))
    (secure-hash 'sha256 (concat outer-pad
                                 (secure-hash 'sha256 (concat inner-pad msg) nil nil t))
                 nil nil nil)))


(defun ycm/send-request (handler json parser callback)
  (let* ((json-data (json-encode json))
         (hmac-b64 (base64-encode-string (ycm/hmac-sha256 ycm/hmac-secret json-data) t)))
    (request
     (format "http://localhost:%d/%s" ycm/ycmd-port handler)
     :type "POST"
     :data json-data
     :headers `(("Content-Type" . "application/json")
                ("X-Ycm-Hmac" . ,hmac-b64))
     :parser parser
     :success callback)))


(defun ycm/hello ()
  (interactive)

  (setq ycm/ycmacs-buffer (get-buffer-create "*ycmacs*"))
  (ycm/log-line "Welcome to ycmacs")

  (let ((settings (ycm/get-default-settings))
        (temp-file (make-temp-file "ycmacs"))
        (json-object-type 'hash-table))

    ;; Generate an auth key, and save the augmented settings to a temp file
    (ycm/generate-hmac-secret)
    (puthash "hmac_secret" (base64-encode-string ycm/hmac-secret t) settings)
    (with-temp-file temp-file (insert (json-encode settings)))

    ;; Start ycmd and link the process to the filter function
    (let ((process-connection-type t))
      (setq ycm/ycmd-process (start-process
                              "ycmd"
                              ycm/ycmacs-buffer
                              ycm/path-to-python2
                              ycm/path-to-ycmd
                              (format "--options_file=%s" temp-file)
                              (format "--idle_suicide_seconds=%d" ycm/server-idle-suicide-seconds))))
    (set-process-query-on-exit-flag ycm/ycmd-process nil)
    (set-process-filter ycm/ycmd-process 'ycm/ycmd-filter)

    ;; Wait until it respons with a port or exits
    (while (and (not ycm/ycmd-port)
                (eq 'run (process-status ycm/ycmd-process)))
      (sleep-for 0 10))

    ;; Make sure the port is set to nil if we got an error
    (if (and ycm/ycmd-port (eq 'run (process-status ycm/ycmd-process)))
        (ycm/log-line (format "ycmd running on port %d" ycm/ycmd-port))
      (ycm/log-line "ycmd failed to start")
      (setq ycm/ycmd-port nil))))


(defun ycm/pre-command ()
  (when ycm/timer
    (cancel-timer ycm/timer)
    (setq ycm/timer nil)))


(defun ycm/post-command ()
  (setq ycm/track-buffer (current-buffer))
  (setq ycm/track-window (selected-window))
  (setq ycm/track-tick (buffer-chars-modified-tick))
  (setq ycm/track-point (point))
  (setq ycm/timer (run-with-timer ycm/idle-delay nil 'ycm/trigger)))


(defun ycm/trigger ()
  (when (and (eq ycm/track-buffer (current-buffer))
             (eq ycm/track-window (selected-window))
             (eq ycm/track-tick (buffer-chars-modified-tick))
             (eq ycm/track-point (point))
             (eq last-command 'self-insert-command))
    (ycm/send-file-ready-to-parse (or buffer-file-name (buffer-name))
                                  (buffer-string) '("unknown"))
    (ycm/send-code-completion-request (or buffer-file-name (buffer-name))
                                      (buffer-string) '("unknown")
                                      (line-number-at-pos) (current-column))
    ))


(defun ycm/send-file-ready-to-parse (filename contents filetypes)
  (ycm/send-request
   "event_notification"
   `(:event_name "FileReadyToParse"
     :filepath ,filename
     :file_data ((,filename
                  . (:contents ,contents
                     :filetypes ,filetypes))))
   nil nil))


(defun ycm/send-code-completion-request (filename contents filetypes line col)
  (ycm/send-request
   "completions"
   `(:filepath ,filename
     :line_num ,line
     :column_num ,col
     :file_data ((,filename
                  . (:contents ,contents
                     :filetypes ,filetypes))))
   'json-read
   (cl-function (lambda (&key data &allow-other-keys)
                  (ycm/handle-completion data)))))


(defun ycm/handle-completion (data)
  (when (and (eq ycm/track-buffer (current-buffer))
             (eq ycm/track-window (selected-window))
             (eq ycm/track-tick (buffer-chars-modified-tick))
             (eq ycm/track-point (point)))
    (let* ((start-column (assq 'completion_start_column data))
           (pt (- (+ (point) 5) (current-column) 1))
           (candidates (mapcar (lambda (q) (cdr (car q)))
                               (cdr (assq 'completions data))))
           )
      (setq ycm/popup (popup-create pt 10 10 :around t))
      (popup-set-list ycm/popup candidates)
      (popup-draw ycm/popup)
      )
    ))



(defun ycm/after-hook ()
  (when (not ycm/ycmd-port)
    (ycm/hello))
  (add-hook 'pre-command-hook 'ycm/pre-command)
  (add-hook 'post-command-hook 'ycm/post-command)
  (make-local-variable 'ycm/popup))


(define-minor-mode ycmacs-mode
  "Ycmacs documentation."
  :init-value nil
  :lighter " ycm"
  :after-hook (ycm/after-hook)
  :keymap nil)


(provide 'ycmacs)

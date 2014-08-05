(require 'json)
(require 'request)

(random t)

(defgroup ycmacs nil
  "Emacs YCM interface")

(defcustom ycm/path-to-python2 "/usr/bin/python2"
  "Path to python2 interpreter"
  :type '(string) :group 'ycmacs)

(defcustom ycm/path-to-ycmd "/home/eivindf/repos/ycmd/ycmd"
  "Path to the ycmd server (location of __main__.py)"
  :type '(string) :group 'ycmacs)

(defcustom ycm/server-idle-suicide-seconds 10800
  "How long to wait idle until the server dies"
  :type '(integer) :group 'ycmacs)

(defcustom ycm/hmac-secret-length 16
  "Length of HMAC secret"
  :type '(integer) :group 'ycmacs)

(defvar ycm/hmac-secret)
(defvar ycm/ycmd-process)
(defvar ycm/ycmd-buffer)
(defvar ycm/ycmd-port)

(defun ycm/log (str)
  (with-current-buffer ycm/ycmd-buffer
    (save-excursion
      (goto-char (point-max))
      (insert str))))

(defun ycm/ycmd-filter (proc str)
  (catch 'ready
    (when (string-match "\\`serving on http://\\([0-9]+\\.\\)\\{3\\}[0-9]+:[0-9]+" str)
      (setq ycm/ycmd-port (string-to-number (car (last (split-string str ":")))))
      (ycm/log (format "ycmd running on port %d\n" ycm/ycmd-port))
      (throw 'ready ""))
    (ycm/log (format "--- %s" str))))

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
      (setq ycm/hmac-secret (base64-encode-string (buffer-string))))))

(defun ycm/hello ()
  (interactive)

  (ycm/generate-hmac-secret)
  (setq ycm/ycmd-buffer (get-buffer-create "*ycmacs*"))
  (ycm/log "Welcome to ycmacs\n")

  (let ((settings (ycm/get-default-settings))
        (temp-file (make-temp-file "ycmacs"))
        (json-object-type 'hash-table))
    (puthash "hmac_secret" ycm/hmac-secret settings)
    (with-temp-file temp-file (insert (json-encode settings)))
    (ycm/log (format "Temporary settings file: %s\n" temp-file))
    (let ((process-connection-type t))
      (setq ycmd-process (start-process
                          "ycmd"
                          ycm/ycmd-buffer
                          ycm/path-to-python2
                          ycm/path-to-ycmd
                          (format "--options_file=%s" temp-file)
                          (format "--idle_suicide_seconds=%d" ycm/server-idle-suicide-seconds))))
    (set-process-filter ycmd-process 'ycm/ycmd-filter)
  ))

(provide 'ycmacs)

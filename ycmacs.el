(require 'json)

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
  (message "Hello from ycmacs")
  (message "Path to ycmd: %s" ycm/path-to-ycmd)

  (ycm/generate-hmac-secret)
  (let ((settings (ycm/get-default-settings))
        (temp-file (make-temp-file "ycmacs"))
        (json-object-type 'hash-table))
    (puthash "hmac_secret" ycm/hmac-secret settings)
    (with-temp-file temp-file (insert (json-encode settings)))
    (message "Written to: %s" temp-file)
    (let ((process-connection-type t))
      (start-process "ycmd"
                     (get-buffer-create "*ycmd*")
                     ycm/path-to-python2
                     ycm/path-to-ycmd
                     (format "--options_file=%s" temp-file)
                     (format "--idle_suicide_seconds=%d" ycm/server-idle-suicide-seconds)))
    )
  )

(provide 'ycmacs)

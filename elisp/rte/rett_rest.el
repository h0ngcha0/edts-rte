;; library for communicating with the rett server
(require 'cl)
(require 'url)
(require 'json)

(defun rett-resource-url (resource args)
  (concat "http://127.0.0.1:8642/" resource "/" args))

(defun rett-rest-request (method resource args &optional body)
  "Send a get request to RESOURCE with ARGS"
  (let ((url                       (rett-resource-url resource args))
        (url-request-method        method)
        (url-request-extra-headers (list '("Content-Type" . "application/json")))
        (url-request-data          body)
        (url-show-status           nil))
    (edts-log-debug "Sending %s-request to %s" method url)
    (let ((buffer (url-retrieve-synchronously url)))
      (when buffer
        (with-current-buffer buffer
          (edts-rest-parse-http-response))))))

(defun rett-rest-post (resource args &optional body)
  (rett-rest-request "POST" resource args body))

(defun rett-rest-put (resource args &optional body)
  (rett-rest-request "PUT" resource args body))

(defun rett-rest-get (resource args &optional body)
  (rett-rest-request "GET" resource args body))

(defun rett-rest-del (resource args &optional body)
  (rett-rest-request "DELETE" resource args body))

(defun rett-construct-editor-body (id x y z code)
  (concat "{\"x\":" x ",\"y\":" y ",\"z\":" z ",\"id\":\"" id "\",\"code\":\"" code "\"}"))

(defun testweb()
  (interactive)
  (let* ((body    (rett-construct-editor-body "10" "10" "10" "200" "niucha"))
         (id      "100"))
    (rett-rest-put-function-body id body)
    ))

(provide 'rett-rest)

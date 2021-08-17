;(defconstant rcl-bin "rclone")
;(defconstant rcl-prm-rmt "listremotes")

; [remote]-remote_name
;
;         -capacity-total
;                  -free
;                  -used
;

(ql:quickload "alexandria")

(defparameter *rclone-remotes-hash* (make-hash-table :test #'equal))

(defun read-stream-ret-list (strm &optional (output))
  "reads from stream and returns list of strings in reversed order"
  ; pure functional and recursive version with tail call 
  (let ((str))
    (if (setf str (read-line strm nil nil))
      (read-stream-ret-list strm (cons str output)) ; list in reverse order because of cons nature
      output)))


(defun run-cmd-get-output (cmd &optional (params))
  "run external command and parse output as a list of strings"
  ; example (* "ls" '("-l" "/"))
  (with-open-stream (out-stream (sb-ext:process-output (sb-ext:run-program cmd params :search t :output :stream :error nil)))
    (read-stream-ret-list out-stream)))
    ;(loop for line = (read-line out-stream nil nil)
    ;      while line collect line)))

;assemble hash with keys as a remotes w/o : symbol 
(loop for x in (run-cmd-get-output "rclone" '("listremotes")) 
      do (setf (gethash (symbol-name (read-from-string (string-trim ":" x))) *rclone-remotes-hash* ) t))
      ; setting (setf (gethash (symbol-name 'YANDEX ) *rclone-remotes-hash* )  "value")
      ; setting (setf (gethash "YANDEX" *rclone-remotes-hash* )  "value")

(loop for x in (alexandria:hash-table-keys *rclone-remotes-hash*)
      do (print (run-cmd-get-output "rclone" '( "about" (concatenate 'string (string-downcase (string x)) ":")))))
;      do (setf (gethash (symbol-name (read-from-string (string-trim ":" x))) *rclone-remotes-hash* ) t))
      ; setting (setf (gethash (symbol-name 'YANDEX ) *rclone-remotes-hash* )  "value")
      ; setting (setf (gethash "YANDEX" *rclone-remotes-hash* )  "value")

(alexandria:hash-table-keys *rclone-remotes-hash*)
(alexandria:hash-table-values *rclone-remotes-hash*)

;(loop for key being the hash-keys of *rclone-remotes-hash* 
;      do (print key))
;(loop for key being the hash-keys of *rclone-remotes-hash* collect key)

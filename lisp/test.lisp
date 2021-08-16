;(defconstant rcl-bin "rclone")
;(defconstant rcl-prm-rmt "listremotes")

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
  (with-open-stream (out-stream (sb-ext:process-output (sb-ext:run-program cmd params :search t :output :stream )))
    (read-stream-ret-list out-stream)))

(defun read-rclone-config ()
  ; read rclone config 
  (defun read-strm-ret-list (strm &optional (output))
    "reads from stream and returns list of strings in reversed order"
    ; pure functional and recursive version with tail call 
    (let ((str))
      (if (setf str (read-line strm nil nil))
        (read-stream-ret-list strm (cons str output)) ; list in reverse order because of cons nature
        output)))
  (with-open-stream (rclone-out-stream (sb-ext:process-output (sb-ext:run-program "rclone" '("listremotes") :search t :output :stream )))
    (read-strm-ret-list rclone-out-stream)))

    ;(loop for line = (read-line rclone-out-stream nil nil)
    ;      while line collect line)))

(defun inn ()
  (incf ii))

(let ((i 0))

;(defun run-cmd-get-output2 (cmd &optional (params))
  ; example (* "ls" '("-l" "/"))
;  (with-open-stream (out-stream (sb-ext:process-output (sb-ext:run-program cmd params :search t :output :stream )))
;    (do ((line t))
;      (eq line nil)
;      (setf line (read-line out-stream nil nil)))))



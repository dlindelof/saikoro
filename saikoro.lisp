(defparameter *board-size* 6)
(defparameter *board-squares-num* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-squares-num* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-squares-num*
                    collect (1+ (random 6)))))
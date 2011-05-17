(defparameter *board-size* 3)
(defparameter *dice-max* 3)
(defparameter *board-squares-num* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-squares-num* :initial-contents lst))

(defun gen-board ()
  (let ((board (board-array (loop for n below *board-squares-num*
                               collect (1+ (random *dice-max*))))))
;    (setf (aref board 0) 'A)
;    (setf (aref board (1- *board-squares-num*)) 'B)
    board))

(defun draw-board (board)
  (fresh-line)
  (format t "|~{~< |~%|~,8:;~2d~>~} |"
          (loop for square across board collect square)))
  
(defun game-tree (player board)
  (list player
        board
        (moves (player board))))

(defun value (pos board)
  (aref board pos))

(defun neighbors (pos board)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
  (loop for p in (append (list up down)
                         (unless (zerop (mod pos *board-size*)) (list (1- pos)))
                         (unless (zerop (mod (1+ pos) *board-size*)) (list (1+ pos))))
       when (and (>= p 0)
                 (< p *board-squares-num*)
                 (> (value p board) 0))
       collect p)))

(defun moves (pos board)
  (defun moves* (pos board n)
    (when (<= n *dice-max*)
      (let ((nb (neighbors pos board)))
        (remove-duplicates (concatenate 'list
                                        (mapcan (lambda (x) (and (= (value x board) n)
                                                                 (list x))) nb)
                                        (mapcan (lambda (x) (moves* x board (1+ n))) nb))))))
  (moves* pos board 1))
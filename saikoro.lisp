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
  (format t "|~{~< |~%|~,8:;~2@A~>~} |"
          (loop for square across board collect square)))
  
(defun game-tree (player board)
  (list player
        board
        (moves player board)))

(defun value (pos board)
  (aref board pos))

(defun player-pos (player board)
  (position player board))

(defun neighbors (pos board)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
  (loop for p in (append (list up down)
                         (unless (zerop (mod pos *board-size*)) (list (1- pos)))
                         (unless (zerop (mod (1+ pos) *board-size*)) (list (1+ pos))))
     when (and (>= p 0)
               (< p *board-squares-num*)
               (numberp (value p board))
               (> (value p board) 0))
       collect p)))

(defun moves (player board)
  (defun moves* (pos board n)
    (when (<= n *dice-max*)
      (let ((nb (neighbors pos board)))
        (remove-duplicates (concatenate 'list
                                        (mapcan (lambda (x) (and (= (value x board) n)
                                                                 (list x))) nb)
                                        (mapcan (lambda (x) (moves* x board (1+ n))) nb))))))
  (mapcar (lambda (m)
            (list m
                  (game-tree (if (eq player 'A) 'B 'A)
                             (move-player player board m))))
          (moves* (player-pos player board) board 1)))

(defun move-player (player board pos)
  (board-array (loop for p below *board-squares-num*
                    for sq across board
                    collect (cond ((= pos p) player)
                                  ((eq sq player) 0)
                                  (t sq)))))
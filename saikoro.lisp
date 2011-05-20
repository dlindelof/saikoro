(defparameter *board-size* 4)
(defparameter *dice-max* 4)
(defparameter *board-squares-num* (* *board-size* *board-size*))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
  (loop for p in (append (list up down)
                         (unless (zerop (mod pos *board-size*)) (list (1- pos)))
                         (unless (zerop (mod (1+ pos) *board-size*)) (list (1+ pos))))
     when (and (>= p 0)
               (< p *board-squares-num*))
       collect p)))

(defparameter *neighbors* (make-array 
                           *board-squares-num*
                           :initial-contents 
                           (loop for p below *board-squares-num*
                              collect (neighbors p))))

(defun board-array (lst)
  (make-array *board-squares-num* :initial-contents lst))

(defun gen-board ()
  (let ((board (board-array (loop for n below *board-squares-num*
                               collect (1+ (random *dice-max*))))))
    (setf (aref board 0) 'A)
    (setf (aref board (1- *board-squares-num*)) 'B)
    board))

(defun draw-board (board)
  (fresh-line)
  (format t "|~{~< |~%|~,26:;~2@A[~2@A]~>~} |"
          (loop for square across board
             for p below *board-squares-num*
             nconc (list square p))))
  
(defun game-tree (player board)
  (list player
        board
        (moves player board)))

(defun value (pos board)
  (aref board pos))

(defun player-pos (player board)
  (position player board))

(defun moves (player board)
  (defun moves* (pos board n)
    (when (<= n *dice-max*)
      (let ((nb (aref *neighbors* pos)))
        (remove-duplicates
         (mapcan (lambda (p)
                   (let ((v (value p board)))
                     (when (numberp v)
                       (append (when (= v n) (list p))
                               (when (> v 0) (moves* p board (1+ n)))))))
                   nb)))))
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

(defun current-moves (tree)
  (third tree))

(defun current-board (tree)
  (second tree))

(defun current-player (tree)
  (first tree))

(defun play-vs-human (tree)
  (print-info tree)
  (if (current-moves tree)
      (play-vs-human (handle-human tree))
      (announce-winner (first tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "Current player: ~A" (current-player tree))
  (draw-board (current-board tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "Choose your move: ")
  (fresh-line)
  (let ((moves (current-moves tree)))
    (loop for move in moves
         for n from 1
         do
          (progn (format t "~A. ~A -> ~A " n (player-pos (current-player tree) (current-board tree)) (first move))
                 (fresh-line)
           ))
    
    (fresh-line)
    (second (nth (1- (read)) moves))))

(defun announce-winner (loser)
  (fresh-line)
  (if (eq 'A loser)
      (format t "B wins!")
      (format t "A wins!")))

(defun rate-position (player tree)
  (let ((moves (current-moves tree)))
    (if moves
        (apply (if (eq (current-player tree)
                       player)
                   #'max
                   #'min)
               (get-ratings player tree))
        (if (eq (current-player tree) player)
            0
            1))))

(defun get-ratings (player tree)
  (mapcar (lambda (move)
            (rate-position player (second move)))
          (current-moves tree)))

(defun handle-computer (tree)
  (let ((ratings (get-ratings (current-player tree) tree)))
    (second (nth (position (apply #'max ratings) ratings) (current-moves tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (current-moves tree)) (announce-winner (current-player tree)))
        ((eq 'A (current-player tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

(defun play ()
  (play-vs-computer (game-tree 'A (gen-board))))

(play)
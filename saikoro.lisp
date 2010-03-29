(defparameter *sample-board*
  '((5 1 3 3 1 2 2 6)
    (1 1 3 6 3 1 5 3)
    (1 2 5 2 6 5 4 4)
    (3 6 3 3 4 3 5 5)
    (4 5 3 1 2 6 3 4)
    (4 6 6 4 3 6 4 6)
    (3 1 4 3 5 2 5 1)
    (6 4 5 6 6 3 1 5))
  "A sample board.")

(defstruct square
  "Representation of a square"
  value
  (neighbors ())
  (dof ()))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the result."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))

(defun remove-square (square)
  "Remove a square from the game"
  (let ((neighbors (square-neighbors square)))
    (dolist (neighbor neighbors)
      (delete square (square-neighbors neighbor)))
    (dolist (neighbor neighbors)
      (update-dof neighbor))))

(defun find-dof (square &optional (depth 0))
  "Find squares that can be reached from this square."
  (cond ((= depth 6) nil)
        ((append (choose-if #'(lambda (square) (= (square-value square) (+1 depth)))
                            (square-neighbors square))
                 (mappend #'(lambda (square) (find-dof square (+ depth 1)))
                          (square-neighbors square))))))

(defun parse-board (board &optional (row-nr 8))
  "Parses a board and returns a list of (row col square)s."
  (when (not (null (first board)))
    (append (parse-row (first board) row-nr)
            (parse-board (rest board) (- row-nr 1)))))

(defun parse-row (row row-nr &optional (col-nr 1))
  (when (not (null (first row)))
    (append (list (list row-nr col-nr (make-square :value (first row))))
            (parse-row (rest row) row-nr (+ 1 col-nr)))))

(defun find-neighbors (parsed-board)
  (mapcar 
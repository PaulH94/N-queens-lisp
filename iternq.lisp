;;;Paul Huynh
;;;10/11/2016
;;;CS441
;;;Lab 1

;Limit around n = 29 queens

;;Function to see if the two current positions are threats to each other
(defun nono (a b c d)                                                           
	(or (= a c)                                                                 
		(= b d)                                                                 
		(= (- a b) (- c d))                                                     
		(= (+ a b) (+ c d))))                                                   
                        
;Checks to see if there is a threat to the current position from any positions
;on the list
(defun conflict (row col q-list)                                                
    (cond                                                                       
		((endp q-list) nil)                                                     
		((nono row col (first (first q-list)) (second (first q-list)))t)        
		(t (conflict row col (rest q-list)))))

;Trying to iteratively figure out where to place the queens on a n by n board
;num is the number of queens that are needed, used for bases cases
;q-list is a list of queens
(defun it-queen (num q-list)
	"Since 2 and 3 queens is not possible"
	(if (or (= num 2) (= num 3))
		(return-from it-queen nil))
	(setf x 1)
	(setf y 1)
	"Loop until a solution is found"
	(loop
		do
		(when (> x num) (return-from it-queen q-list))
		(cond
			((> y num) (setf x (first (first q-list))) (setf y (+ 1 (second (first q-list)))) (setf q-list (rest q-list))) 
			((conflict x y q-list) (setf y (1+ y)))
			(t (setf q-list (cons (list x y) q-list)) (setf x (1+ x)) (setf y 1)))))


;This function is to be used as a display and wrapper function for the it-queen
;function.
(defun place-q (num)
	(princ "The number of queens: " )
	(write num)
	(terpri)
	(princ "This is the set of queens: ")
	(setf q-list (list))
	(write (it-queen num q-list))
	(terpri))

;This function will allow you to solve every single n-queens puzzle up to num.
(defun queens-until (num)
	(write-line "This program's purpose is to find a solution for the n-queens problem.")
	(loop for x from 1 to num
		do
		(place-q x)
		(terpri)))


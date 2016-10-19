;;;Paul Huynh
;;;10/11/2016
;;;CS441
;;;Lab 1

;Limit around n = 19 queens

;Function to see if the two current positions are threats to each other
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

;Figuring out recursively on where to place the queens on the board of n by n
;row is to keep track of the current row, and the same goes for col & column
;cap is the n in the n-queens problem, is used for base cases.
;q-list will keep track of the current list of queens.
(defun rec-q (row col cap q-list)                                          
	(cond           
		((> row cap) q-list)                                                    
		((> col cap) nil) 
		((conflict row col q-list) (rec-q row (+ 1 col) cap q-list))
		((eq nil (rec-q (+ 1 row) 1 cap (cons (list row col) q-list))) (rec-q row (+ 1 col) cap q-list))
		(t (rec-q (+ 1 row) 1 cap (cons (list row col) q-list)))))

;This function is to be used as a display and wrapper function for the rec-q
;function.
(defun place-q (num)
	(princ "The number of queens: " )
	(write num)
	(terpri)
	(princ "This is the set of queens: ")
	(setf q-list (list))
	;(write (it-queen num q-list)))
	(write (rec-q 1 1 num q-list))
	(terpri))

;This function will allow you to solve every single n-queens puzzle up to num.
(defun queens-until (num)
	(write-line "This program's purpose is to find a solution for the n-queens problem.")
		(loop for x from 1 to num
			do
			(place-q x)
			(terpri)))

;This function will print out all solutions for the n-queens
(defun rec-queens (row col cap q-list)
	(cond 
		((> row cap) q-list)
		((> col cap) nil)
		((conflict row col q-list) (rec-queens row (+ 1 col) cap q-list))
		(t (setf temp (list row col)) (cond 
										((and (> cap row) (eq nil (rec-queens (+ 1 row) 1 cap (cons temp q-list)))) 																(rec-queens row (+ 1 col) cap q-list)) 
										(t (rec-queens (+ 1 row) 1 cap (cons temp q-list)))))))

(defun place-queens (num)                                                            
	(princ "The number of queens: " )                                           
	(write num)                                                                 
	(terpri)                                                                    
	(princ "This is the set of queens: ")                                       
	(setf q-list (list))                                                        
	(write (rec-queens 1 1 num q-list))                                              
	(terpri))  

(place-queens 1)
(place-queens 4)

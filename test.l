; test everything
(defun test ()
	(format t "~%~10TBEGIN TESTING~2%")
	(test-order)
	(test-combine)
	(test-remove-if)
	(test-simplify)
	(format t "~%~10TDONE TESTING~2%")
	t
)

; some shorthands, so that you can just type, e.g., (td)
(defun td () (test-order))
(defun tc () (test-combine))
(defun tr () (test-remove-if))
(defun ts () (test-simplify))

; use function ty -- i.e., (ty) -- to test everything and create Output.your
; then exit from clisp.
; then do a "diff Output.correct Output.your"
; files must be identical,
; except as noted on the assignment handout. 
;
; Output.correct was generated after
; (setq *print-case* :downcase)
; so, you might want to invoke clisp via "clisp -q -i init.lsp"
; where init.lsp is in Given.
;
; NB:  clisp output formatting takes into account the number of columns
; in your window.  The Output.correct file was generated
; for 80 column output.  Your output may differ for different sized windows.
; It's okay if your output is the same except for whitespace formatting
; (but easier, so you can use diff, if you run in an 80 column window).

; delete-file is needed because some clisps (e.g., the one on Linux PC)
; append rather than truncate when dribbling to an existing file.
(defun ty ()
        (delete-file "Output.your") (dribble "Output.your") (test) (dribble))

(defun test-order ()
	(format t "~%~20TTEST-ORDER~2%")
	(tester '(order nil nil))
	(tester '(order 'a nil))
	(tester '(order nil 44))
	(tester '(order nil '(1 2 3 4)))
	(tester '(order '(1 2 3 4) '(1 2 3 4)))
	(tester '(order 1 2))
	(tester '(order 2 2))
	(tester '(order 2 1))
	(tester '(order 0 -999))
	(tester '(order '(10) '(10)))
	(tester '(order '10 '10))
	t
)

; test all the combine functions
(defun test-combine ()
	(format t "~%~20TTEST-COMBINE~2%")
	(test-combine-recursive)
	(test-combine-mapcar)
	(test-combine-iterative)
	t
)
(defun test-combine-recursive ()
	(test-a-combine 'combine-recursive)
	t
)
(defun test-combine-mapcar ()
	(test-a-combine 'combine-mapcar)
	t
)
(defun test-combine-iterative ()
	(test-a-combine 'combine-iterative)
	t
)

; tests the combine function combine-funx.
; prints out combine-funx instead of actual name,
; e.g., (combine-funx nil nil) instead of (combine-recursive nil nil)
;
(defun test-a-combine (combine-funx)
	(format t "~%~30TTEST-A-COMBINE on:  ~A~2%" combine-funx)
	(defun combine-fun (x y) (funcall combine-funx x y))
	(tester '(combine-fun nil nil))
	(tester '(combine-fun 'a nil))
	(tester '(combine-fun nil 44))
	(tester '(combine-fun nil '(1 2 3 4)))
	(tester '(combine-fun '(1) '(2)))
	(tester '(combine-fun '(1 2) '(3 4)))
	(tester '(combine-fun '(1 2) '(1 2)))
	(tester '(combine-fun '(3 4) '(1 2)))
	(tester '(combine-fun '(1 2 3) '()))
	(tester '(combine-fun '(3 2 1) '(1 2 3)))
	(tester '(combine-fun '(1 2 2 3) '(0 0 2 3)))
	(tester '(combine-fun '(1 2 0 -3) '(0 0 2 3)))
	(tester '(combine-fun '(1 2 0 -3 4) '(0 -1 2 3 4)))
	(tester '(combine-fun '(1 2 3 4) '(5)))
	(tester '(combine-fun '(1 2 3 4) nil))
	(tester '(combine-fun nil '(1 2 3 4)))
	t
)

(defun test-remove-if ()
	(format t "~%~20TTEST-REMOVE-IF~2%")
	(test-a-remove-if 'remove-if-recursive)
	(test-a-remove-if 'remove-if-mapcar)
	t
)

(defun test-a-remove-if (remove-if-funx)
	(format t "~%~30TTEST-A-REMOVE-IF on:  ~A~2%" remove-if-funx)
	(defun remove-if-fun (x y) (funcall remove-if-funx x y))
	(tester '(remove-if-fun 'evenp '(0 1 2 3 4)))
	(tester '(remove-if-fun 'oddp '(0 1 2 3 4)))
	(tester '(remove-if-fun 'evenp '(0 2 4 6 8 10 12)))
	(tester '(remove-if-fun 'oddp '(0 2 4 6 8 10 12)))
	(tester '(remove-if-fun 'evenp nil))
	(tester '(remove-if-fun 'listp '(0 nil (a b c) hi (()) mom)))
	(tester '(remove-if-fun
	        #'(lambda (x) (= (length x) 3)) '( (a) (a b c) () (d e f))))
	(tester '(remove-if-fun
	        #'(lambda (x) (< (length x) 3)) '( (a) (a b c) () (d e f))))
	t
)

(defun test-simplify ()
	(format t "~%~20TTEST-SIMPLIFY~2%")
	(tester '(simplify nil))
	(tester '(simplify '(a b c d e f)))
	(tester '(simplify '(not a)))
	(tester '(simplify '(not (not a))))
	(tester '(simplify '(not (not (not a)))))
	(tester '(simplify '(not (not (not (not a))))))
	(tester '(simplify '(not (not (not (not (not a)))))))
	(tester '(simplify '(cons (not (not (not a))) (not (not b)))))
	(tester '(simplify '(cons (not (goo (not a))) (not (foo b)))))
	(tester '(simplify '(cons (not (not (goo a))) (foo (not b)))))
	(tester '(simplify '(cons (not (not (goo (not (not (not a))))))
				(foo (not (not (not (foo (not (not b))))))))))
	(tester '(simplify '(and a b)))
	(tester '(simplify '(or b)))
	(tester '(simplify '(and (or a b))))
	(tester '(simplify '(and (and a b))))
	(tester '(simplify '(goo (and (and a b)))))
	(tester '(simplify '(and a (and b c) d (and e f))))
	(tester '(simplify '(and a (and b (and c1 c2 c3)) d (and e f))))
	(tester '(simplify '(and a (or b (or c1 c2 c3)) d (and e f))))
	(tester '(simplify '(and a (goo b (or c1 c2 c3)) d (and e f))))
	(tester '(simplify
		 '(and a
		      (goo b (or c1 c2 c3))
		      (foo (and (and b c) d) (and e f)))))
	(tester '(simplify
		 '(and a (goo b (or (not (not c1)) (not (not (not c2))) c3))
		      (foo (and (and b c) d) (and e f)
			   (not (and (not x) (or (or (not (not x))))))))))
	(tester '(simplify '(and a (not (not (and b c))) d)))
	(tester '(simplify '(foo (and a (not (not (and b c))) d))))
	(tester '(simplify '(foo (and a (not (not (and b c))) d
				    (and e (not (not (and f g))) h)))))
	(tester '(simplify
		 '(foo (or a (not (not (or b (not (not (or c d)))))) e))))
	(tester '(simplify
		 '(foo (or a (not (not (and b (not (not (or c d)))))) e))))
	(tester '(simplify
		 '(foo (or a (not (and b (not (not (not (or c d))))))) e)))
	; something like following would be interesting,
	; but alas it will give away my code
	; if I run it and make its output available...
	; (tester '(simplify
        ;             (third (fourth (function-lambda-expression 'simplify)))))

	t
)

; low-level testing routine
(defun tester (x)
	(format t "testing:  ~A~%" x)
	; split into 2 so print value even if eval fails.
	(format t "value:    ")
	(format t "~A~2%" (eval x))
	t
)

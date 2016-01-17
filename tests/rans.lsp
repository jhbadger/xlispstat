(defun get-random-state-values (s)
  (second (read-from-string (prin1-to-string s) t nil :start 2)))

(defun test-random-state (state values)
  (let ((*random-state* state)
	(state-max (aref (get-random-state-values *random-state*) 0)))
    (check #'=
	   0
	   (mapcar #'(lambda (v)
		       (random state-max)
		       (- v (aref (get-random-state-values *random-state*) 1)))
		   values))))

; combined congruential
(test-random-state '#$(1 #(2147483562 0 12345 67890))
		   '(2026359911 1950599823 315009702 1105313978 871469535
		     1575849876   94472070 728775444 2137747604 430227419))

; superduper
(test-random-state '#$(2 #(2147483647 0 0 12345 1 2354))
		   '(274318794 1445882217 174694986 21258797 860839904
		     1611788216 2056178182 1140888495 12651638 485726963))

; combined tausworth
(test-random-state '#$(3 #(2147483647 0 12345 67890))
		   '(702677384 51837197 1914342640 434186539 1381102196
		     1693194944 898066022 1288590140 645917234 1416028832))




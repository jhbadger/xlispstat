;;;;
;;;;
;;;;                  A Simple GLIM Implementation
;;;;
;;;;

(provide "glim")

;;;;
;;;;                         Link Prototypes
;;;;
;;;;
;;;; Links are objects responding to three messages:
;;;;
;;;;     :eta     takes a set of mean values and returns the linear
;;;;              predictor values
;;;;     :means   takes a set of linear predictor values and returns
;;;;              the mean values (the inverse of :eta)
;;;;     :derivs  takes a set of mean values and returns the values of
;;;;              the derivatives of the linear predictors at the mean
;;;;              values
;;;;
;;;; The arguments should be sequences. The glim-link-proto prototype
;;;; implements an identity link. Links for binomial errors are defined
;;;; for means in the unit interval [0, 1], i. e. for n = 1 trials.
;;;;

(defproto glim-link-proto)

(defmeth glim-link-proto :eta (mu)
"Method args: (mu)
Returns linear predictor values at MU."
  mu)

(defmeth glim-link-proto :means (eta)
"Method args: (eta)
Returns mean values for linear predictor ETA."
  eta)

(defmeth glim-link-proto :derivs (mu)
"Method args: (mu)
Returns d(eta)/d(mu) values at MU."
  (repeat 1 (length mu)))

(defmeth glim-link-proto :print (&optional (stream t))
  (format stream "#<Glim Link Object: ~s>" (slot-value 'proto-name)))

(defmeth glim-link-proto :save ()
  (let ((proto (slot-value 'proto-name)))
    (if (eq self (eval proto)) proto `(send ,proto :new))))

;;;;
;;;; Identity Link Prototype
;;;;

(defproto identity-link () () glim-link-proto)

;;;;
;;;; Log Link Prototype
;;;;

(defproto log-link () () glim-link-proto)

(defmeth log-link :eta (mu) (log mu))
(defmeth log-link :means (eta) (exp eta))
(defmeth log-link :derivs (mu) (/ mu))

;;;;
;;;; Inverse-link-prototype
;;;;

(defproto inverse-link () () glim-link-proto)

(defmeth inverse-link :eta (mu) (/ mu))
(defmeth inverse-link :means (eta) (/ eta))
(defmeth inverse-link :derivs (mu) (- (/ (^ mu 2))))

;;;;
;;;; Square Root Link
;;;;

(defproto sqrt-link () () glim-link-proto)

(defmeth sqrt-link :eta (mu) (sqrt mu))
(defmeth sqrt-link :means (eta) (^ eta 2))
(defmeth sqrt-link :derivs (mu) (/ 0.5 (sqrt mu)))

;;;;
;;;; Power Link Prototype
;;;;

(defproto power-link-proto '(power) () glim-link-proto)

(defmeth power-link-proto :isnew (power) (setf (slot-value 'power) power))
(defmeth power-link-proto :power () (slot-value 'power))

(defmeth power-link-proto :print (&optional (stream t))
  (format stream "#<Glim Link Object: Power Link (~s)>" (send self :power)))

(defmeth power-link-proto :save ()
  `(send power-link-proto :new ,(send self :power)))

(defmeth power-link-proto :eta (mu) (^ mu (send self :power)))
(defmeth power-link-proto :means (eta) (^ eta (/ (slot-value 'power))))
(defmeth power-link-proto :derivs (mu)
  (let ((p (slot-value 'power)))
    (* p (^ mu (- p 1)))))

;;;;
;;;; Logit Link Prototype
;;;;

(defproto logit-link () () glim-link-proto)

(defmeth logit-link :eta (p) (log (/ p (- 1 p))))
(defmeth logit-link :means (eta)
  (let ((exp-eta (exp eta)))
    (/ exp-eta (+ 1 exp-eta))))
(defmeth logit-link :derivs (p) (+ (/ p) (/ (- 1 p))))

;;;;
;;;; Probit Link Prototype
;;;;

(defproto probit-link () () glim-link-proto)

(defmeth probit-link :eta (p) (normal-quant p))
(defmeth probit-link :means (eta) (normal-cdf eta))
(defmeth probit-link :derivs (p) (/ 1 (normal-dens (normal-quant p))))

;;;;
;;;; Complimentary Log-Log Link Prototype
;;;;

(defproto cloglog-link () () glim-link-proto)

(defmeth cloglog-link :eta (p) (log (- (log (- 1 p)))))
(defmeth cloglog-link :means (eta) (- 1 (exp (- (exp eta)))))
(defmeth cloglog-link :derivs (p)
  (let ((q (- 1 p)))
    (/ -1 (log q) q)))

;;;;
;;;;
;;;;                The General GLIM Prototype
;;;;         (Uses Normal Errors and an Identity Link)
;;;;
;;;;

(defproto glim-proto 
  '(yvar link offset pweights scale est-scale
         epsilon epsilon-dev count-limit verbose recycle
         eta deviances)
  '() 
  regression-model-proto)

;;;;
;;;; Slot Accessors
;;;;

(defmeth glim-proto :yvar (&optional (new nil set))
"Message args: (&optional new)
Sets or returns dependent variable."
  (when set
        (setf (slot-value 'yvar) new)
        (send self :needs-computing t))
  (slot-value 'yvar))

(defmeth glim-proto :link (&optional (new nil set))
"Message args: (&optional new)
Sets or returns link object."
  (when set
        (setf (slot-value 'link) new)
        (send self :needs-computing t))
  (slot-value 'link))

(defmeth glim-proto :offset (&optional (new nil set))
"Message args: (&optional (new nil set))
Sets or returns offset values."
  (when set
        (setf (slot-value 'offset) new)
        (send self :needs-computing t))
  (slot-value 'offset))

(defmeth glim-proto :pweights (&optional (new nil set))
"Message args: (&optional (new nil set))
Sets or returns prior weights."
  (when set
        (setf (slot-value 'pweights) new)
        (send self :needs-computing t))
  (slot-value 'pweights))

;; changing the scale does not require recomputing the estimates
(defmeth glim-proto :scale (&optional (new nil set))
"Message args: (&optional (new nil set))
Sets or returns value of scale parameter."
  (if set (setf (slot-value 'scale) new))
  (slot-value 'scale))

(defmeth glim-proto :estimate-scale (&optional (val nil set))
"Message args: (&optional (val nil set))
Sets or returns value of ESTIMATE-SCALE option."
  (if set (setf (slot-value 'est-scale) val))
  (slot-value 'est-scale))

(defmeth glim-proto :epsilon (&optional new)
"Message args: (&optional new)
Sets or returns tolerance for relative change in coefficients."
  (if new (setf (slot-value 'epsilon) new))
  (slot-value 'epsilon))

(defmeth glim-proto :epsilon-dev (&optional new)
"Message args: (&optional new)
Sets or returns tolerance for change in deviance."
  (if new (setf (slot-value 'epsilon-dev) new))
  (slot-value 'epsilon-dev))

(defmeth glim-proto :count-limit (&optional new)
"Message args: (&optional new)
Sets or returns maximum number of itrations."
  (if new (setf (slot-value 'count-limit) new))
  (slot-value 'count-limit))

(defmeth glim-proto :recycle (&optional (new nil set))
"Message args: (&optional new)
Sets or returns recycle option. If option is not NIL, current values
are used as initial values by :COMPUTE method."
  (when set
        (setf (slot-value 'recycle) new))
  (slot-value 'recycle))

(defmeth glim-proto :verbose (&optional (val nil set))
"Message args: (&optional (val nil set))
Sets or returns VERBOSE option. Iteration info is printed if option
is not NIL."
  (if set (setf (slot-value 'verbose) val))
  (slot-value 'verbose))

(defmeth glim-proto :eta ()
"Message args: ()
Returns linear predictor values for durrent fit."
  (slot-value 'eta))

(defmeth glim-proto :set-eta (&optional val)
  (if val
      (setf (slot-value 'eta) val)
      (setf (slot-value 'eta)
            (+ (send self :offset) (send self :fit-values)))))

(defmeth glim-proto :deviances ()
"Message args: ()
Returns deviances for durrent fit."
  (slot-value 'deviances))

(defmeth glim-proto :set-deviances ()
  (setf (slot-value 'deviances) 
        (send self :fit-deviances (send self :fit-means))))

;;;;
;;;; Overrides for Regression Methods
;;;;

;; A variant of this method should work for any object whose slot values
;; have valid printed representations.
(defmeth glim-proto :save ()
  (let* ((proto (slot-value 'proto-name))
	 (slots (remove 'link (send self :own-slots)))
	 (values (mapcar #'slot-value slots)))
    `(let ((object (make-object ,proto))
	   (slots ',slots)
	   (values ',values))
       (flet ((add-slot (s v) (send object :add-slot s v)))
	 (mapcar #'add-slot slots values)
	 (add-slot 'link ,(send (send self :link) :save)))
       object)))

(defmeth glim-proto :sigma-hat () (sqrt (send self :scale)))

;; This override is only used to modify the documentation string.
(defmeth glim-proto :fit-values ()
"Message args: ()
Returns Xb, the linear predictor values without the offset.
The :fit-means method returns fitted means for the current estimates."
  (call-next-method))

;; this should be merged with the regression-model method
(defmeth glim-proto :x (&optional x)
  (if x
      (let ((x (cond
                ((matrixp x) x)
                ((vectorp x) (list x))
                ((and (consp x) (numberp (car x))) (list x))
                (t x))))
        (call-next-method (if (matrixp x) x (apply #'bind-columns x)))))
  (call-next-method))

(defmeth glim-proto :raw-residuals () 
"Message args: ()
Returns the raw residuals for a model."
  (- (send self :yvar) (send self :fit-means)))

;; This override is needed because regression-model-proto defines its
;; residuals in terms of :raw-residuals.
(defmeth regression-model-proto :residuals ()
"Message args: ()
Returns the Pearson residuals."
  (let ((raw-residuals (- (send self :y) (send self :fit-values)))
        (weights (send self :weights)))
    (if weights (* (sqrt weights) raw-residuals) raw-residuals)))

;;;;
;;;; Computing methods
;;;;

(defmeth glim-proto :compute ()
  (let* ((epsilon (send self :epsilon))
         (epsilon-dev (send self :epsilon-dev))
         (maxcount (send self :count-limit))
         (low-lim (* 2 (/ machine-epsilon epsilon)))
         (verbose (send self :verbose)))
    (unless (and (send self :eta) (send self :recycle))
            (send self :initialize-search))
    (send self :compute-step)
    (do ((count 1 (+ count 1))
         (beta 0 (send self :coef-estimates))
         (last-beta -1 beta)
         (dev  0 (send self :deviance))
         (last-dev  -1 dev))
        ((or (> count maxcount) 
             (< (max (abs (/ (- beta last-beta)
                             (pmax (abs last-beta) low-lim))))
                epsilon)
             (< (abs (- dev last-dev)) epsilon-dev)))
        (if verbose 
            (format t "Iteration ~d: deviance = ~,6g~%" 
                    count (send self :deviance)))
        (send self :compute-step))))

(defmeth glim-proto :compute-step ()
"Args: ()
Executes one iteratively reweighted least squares step."
  (let* ((yvar (send self :yvar))
         (offset (send self :offset))
         (eta (send self :eta))
         (mu (send self :fit-means eta))
         (d-eta (send self :fit-link-derivs mu))
         (z (- (+ eta (* (- yvar mu) d-eta)) offset))
         (v (send self :fit-variances mu))
         (w-inv (* d-eta d-eta v))
         (pw (send self :pweights)))
    (send self :y z)
    (send self :weights (if pw (/ pw w-inv) (/ w-inv)))
    (call-method regression-model-proto :compute)
    (send self :set-eta)
    (send self :set-deviances)
    (if (send self :estimate-scale) 
        (send self :scale (send self :fit-scale)))))

(defmeth glim-proto :deviance () 
"Message args: ()
Returns deviance for included cases."
  (sum (if-else (send self :included) (send self :deviances) 0)))

(defmeth glim-proto :mean-deviance ()
"Message args: ()
Returns mean deviance for included cases, adjusted for degrees of
freedom."
  (/ (send self :deviance) (send self :df)))

(defmeth glim-proto :initialize-search (&optional eta)
  (send self :set-eta
	(if eta eta (send (send self :link) :eta (send self :initial-means))))
  (send self :needs-computing t))

(defmeth glim-proto :fit-means (&optional (eta (send self :eta)))
"Message args: (&optional (eta (send self :eta)))
Retruns mean values for current or supplied ETA."
  (send (send self :link) :means eta))

(defmeth glim-proto :fit-link-derivs (mu)
"Message args: ()
Returns link derivative values at MU."
  (send (send self :link) :derivs mu))

(defmeth glim-proto :display ()
"Message args: ()
Prints the IRWLS regression summary. Variables not used in the fit are
marked as aliased."
  (let ((coefs (coerce (send self :coef-estimates) 'list))
        (se-s (send self :coef-standard-errors))
        (x (send self :x))
        (p-names (send self :predictor-names)))
    (if (send self :weights) 
        (format t "~%Weighted Least Squares Estimates:~2%")
        (format t "~%Least Squares Estimates:~2%"))
    (when (send self :intercept)
          (format t "Constant~25t~13,6g~40t(~,6g)~%" (car coefs) (car se-s))
          (setf coefs (cdr coefs))
          (setf se-s (cdr se-s)))
    (dotimes (i (array-dimension x 1)) 
             (cond 
               ((member i (send self :basis))
                (format t "~a~25t~13,6g~40t(~,6g)~%"
                        (select p-names i) (car coefs) (car se-s))
                (setf coefs (cdr coefs) se-s (cdr se-s)))
               (t (format t "~a~25taliased~%" (select p-names i)))))
    (format t "~%")
    (if (send self :estimate-scale)
        (format t "Scale Estimate:~25t~13,6g~%" (send self :scale))
        (format t "Scale taken as:~25t~13,6g~%" (send self :scale)))
    (format t "Deviance:~25t~13,6g~%" (send self :deviance))
    (format t "Number of cases:~25t~9d~%" (send self :num-cases))
    (if (/= (send self :num-cases) (send self :num-included))
        (format t "Number of cases used:~25t~9d~%" (send self :num-included)))
    (format t "Degrees of freedom:~25t~9d~%" (send self :df))
    (format t "~%")))


;;;;
;;;; Error-Dependent Methods (Normal Errors)
;;;;

(defmeth glim-proto :initial-means ()
"Message args: ()
Returns initial means estimate for starting the iteration."
  (send self :yvar))

(defmeth glim-proto :fit-variances (mu)
"Message args: (mu)
Returns variance function values at MU."
  (repeat 1 (length mu)))

(defmeth glim-proto :fit-deviances (mu)
"Message args: (mu)
Returns deviance values at MU."
  (let ((raw-dev (^ (- (send self :yvar) mu) 2))
                  (pw (send self :pweights)))
       (if pw (* pw raw-dev) raw-dev)))

(defmeth glim-proto :fit-scale ()
"Message args: ()
Returns estimate of scale parameter."
  (send self :mean-deviance))

;;;;
;;;; Initial values for the prototype
;;;;

(send glim-proto :scale 1.0)
(send glim-proto :offset 0.0)
(send glim-proto :link identity-link)
(send glim-proto :estimate-scale t)
(send glim-proto :epsilon .000001)
(send glim-proto :epsilon-dev .001)
(send glim-proto :count-limit 30)
(send glim-proto :verbose t)

;;;;
;;;; :ISNEW method
;;;;

(defmeth glim-proto :isnew (&key x 
                                 y
				 link
                                 (offset 0)
                                 (intercept t)
                                 included
                                 pweights
                                 (print (and x y))
                                 (verbose t)
                                 predictor-names
                                 response-name
                                 (recycle nil)
                                 case-labels)
  (send self :x x)
  (send self :y y)
  (send self :yvar y)
  (if link (send self :link link))
  (send self :offset offset)
  (send self :intercept intercept)
  (send self :pweights pweights)
  (send self :recycle recycle)
  (send self :verbose verbose)
  (if included (send self :included included))
  (if predictor-names (send self :predictor-names predictor-names))
  (if response-name (send self :response-name response-name))
  (if (or y case-labels) (send self :case-labels case-labels)) ; needs fixing
  (if print (send self :display)))

;;;;
;;;; Some Additional Residual Methods
;;;;

(defmeth glim-proto :chi-residuals ()
"Message args: ()
Returns the components of Pearson's chi-squared residuals."
  (send self :residuals))

(defmeth glim-proto :standardized-chi-residuals ()
"Message args: ()
Returns the components of Standardized Pearson Residuals (Williams, 1987)."
  (send self :studentized-residuals))

(defmeth glim-proto :deviance-residuals ()
"Message args: ()
Returns the components of deviance residuals for non binomial models."
  (let* ((dev (sqrt (send self :deviances)))
         (sign (if-else (< (send self :yvar) (send self :fit-means)) -1 1)))
    (* sign dev)))

(defmeth glim-proto :standardized-deviance-residuals ()
"Message args: ()
Returns the standardized deviance residuals, (Davison and Tsai, 1989)."
  (let* ((dev (send self :deviance-residuals))
         (inc (send self :included))
         (h (send self :leverages)))
    (if-else inc
             (/ dev (sqrt (* (send self :scale) (- 1 h))))
             (/ dev (sqrt (* (send self :scale) (+ 1 h)))))))

(defmeth glim-proto :g2-residuals ()
"Message args: ()
Returns  a weighted combination of the standardized deviance and chi
residuals, (Davison and Tsai, 1989)."
  (let* ((dev (send self :standardized-deviance-residuals))
         (chi (send self :standardized-chi-residuals))
         (inc (send self :included))
         (h (send self :leverages))
         (sign (if-else (< dev 0) -1 1)))
    (* sign (sqrt (+ (* (- 1 h) (^ dev 2))
                     (*   h    (^ chi 2)))))))

;;;;
;;;;
;;;;                 Normal Regression Model Prototype
;;;;
;;;;

(defproto normalreg-proto () () glim-proto)

;;;;
;;;; Normal Model Constructor Function
;;;;

(defun normalreg-model (x y &rest args)
"Args: (x y &rest args)
Returns a normal regression model. Accepts :LINK, :OFFSET and :VERBOSE
keywords in addition to the keywords accepted by regression-model."
  (apply #'send normalreg-proto :new :x x :y y args))

;;;;
;;;;
;;;;             Poisson Regression Model Prototype
;;;;
;;;;

(defproto poissonreg-proto () () glim-proto)

;;;;
;;;; Error-Dependent Methods (Poisson Errors)
;;;;

(defmeth poissonreg-proto :initial-means () (pmax (send self :yvar) 0.5))

(defmeth poissonreg-proto :fit-variances (mu) mu)

(defmeth poissonreg-proto :fit-deviances (mu)
  (flet ((log+ (x) (log (if-else (< 0 x) x 1)))) ; to prevent log of zero
    (let* ((y (send self :yvar))
           (raw-dev (* 2 (- (* y (log+ (/ y mu))) (- y mu))))
           (pw (send self :pweights)))
      (if pw (* pw raw-dev) raw-dev))))

;;;;
;;;; Initial values for the prototype
;;;;

(send poissonreg-proto :estimate-scale nil)
(send poissonreg-proto :link log-link)

;;;;
;;;; Poisson Model Constructor Functions
;;;;

(defun poissonreg-model (x y &rest args)
"Args: (x y &rest args)
Returns a Poisson regression model. Accepts :LINK, :OFFSET and :VERBOSE
keywords in addition to the keywords accepted by regression-model."
  (apply #'send poissonreg-proto :new :x x :y y args))

(defun loglinreg-model (x y &rest args)
"Args: (x y &rest args)
Returns a Poisson regression model with a log link. Accepts :OFFSET and
:VERBOSE keywords in addition to the keywords accepted by regression-model."
  (apply #'send poissonreg-proto :new :x x :y y :link log-link args))

;;;;
;;;;
;;;;             Binomial Regression Model Prototype
;;;;
;;;;

(defproto binomialreg-proto '(trials) () glim-proto)

;;;;
;;;; Slot Accessor
;;;;

(defmeth binomialreg-proto :trials (&optional new)
"Message args: ()
Sets or retruns number of trials for each observation."
  (when new
        (setf (slot-value 'trials) new)
        (send self :needs-computing t))
  (slot-value 'trials))

;;;;
;;;; Overrides for link-related methods to incorporate trials
;;;;

(defmeth binomialreg-proto :fit-means (&optional (eta (send self :eta)))
  (let ((n (send self :trials))
	(p (call-next-method eta)))
    (* n p)))

(defmeth  binomialreg-proto :fit-link-derivs (mu)
  (let* ((n (send self :trials))
	 (d (call-next-method (/ mu n))))
    (/ d n)))

(defmeth binomialreg-proto :initialize-search (&optional eta)
  (call-next-method 
   (if eta eta (send (send self :link) :eta (send self :initial-probs)))))

;;;;
;;;; Error-Dependent Methods (Binomial Errors)
;;;;

(defmeth binomialreg-proto :initial-probs ()
  (let* ((n (send self :trials))
	 (p (/ (pmax (pmin (send self :yvar) (- n 0.5)) 0.5) n)))
    p))

(defmeth binomialreg-proto :initial-means ()
  (* (send self :trials) (send self :initial-probs)))

(defmeth binomialreg-proto :fit-variances (mu)
  (let* ((n (send self :trials))
         (p (/ mu n)))
    (* n p (- 1 p))))

(defmeth binomialreg-proto :fit-deviances (mu)
  (flet ((log+ (x) (log (if-else (< 0 x) x 1)))) ; to prevent log of zero
    (let* ((n (send self :trials))
           (y (send self :yvar))
           (n-y (- n y))
           (n-mu (- n mu))
           (pw (send self :pweights))
           (raw-dev (* 2 (+ (* y (log+ (/ y mu))) 
                            (* n-y (log+ (/ n-y n-mu)))))))
      (if pw (* pw raw-dev) raw-dev))))

;;;;
;;;; Other Methods
;;;;

(defmeth binomialreg-proto :fit-probabilities ()
"Message args: ()
Returns the fitted probabilities for the model."
  (/ (send self :fit-means) (send self :trials)))

;;;;
;;;; :ISNEW method
;;;;

(defmeth binomialreg-proto :isnew (&rest args &key trials)
  (send self :trials trials)
  (apply #'call-next-method args))

;;;;
;;;; Initial values for the prototype
;;;;

(send binomialreg-proto :estimate-scale nil)
(send binomialreg-proto :link logit-link)

;;;;
;;;; Binomial Model Constructor Functions
;;;;

(defun binomialreg-model (x y n &rest args)
"Args: (x y n &rest args)
Returns a binomial regression model. Accepts :LINK, :OFFSET and :VERBOSE
keywords in addition to the keywords accepted by regression-model."
  (apply #'send binomialreg-proto :new :x x :y y :trials n args))

(defun logitreg-model (x y n &rest args)
"Args: (x y n &rest args)
Returns a logistic regression model (binomial regression model with logit
link). Accepts :OFFSET and :VERBOSE keywords in addition to the keywords
accepted by regression-model."
  (apply #'send binomialreg-proto :new 
	 :x x :y y :trials n :link logit-link args))

(defun probitreg-model (x y n &rest args)
"Args: (x y n &rest args)
Returns a probit regression model (binomial regression model with probit
link). Accepts :OFFSET and :VERBOSE keywords in addition to the keywords
accepted by regression-model."
  (apply #'send binomialreg-proto :new
	 :x x :y y :trials n :link probit-link args))

;;;;
;;;;
;;;;               Gamma Regression Model Prototype
;;;;
;;;;

(defproto gammareg-proto () () glim-proto)

;;;;
;;;; Error-Dependent Methods
;;;;

(defmeth gammareg-proto :initial-means () (pmax (send self :yvar) 0.5))

(defmeth gammareg-proto :fit-variances (mu) (^ mu 2))

(defmeth gammareg-proto :fit-deviances (mu)
  (let* ((y (send self :yvar))
	 (pw (send self :pweights))
         (raw-dev (* 2 (+ (- (log (/ y mu))) (/ (- y mu) mu)))))
    (if pw (* raw-dev pw) raw-dev)))

;;;;
;;;; Initial values for the prototype
;;;;

(send gammareg-proto :link inverse-link)

;;;;
;;;; Gamma Model Constructor Function
;;;;

(defun gammareg-model (x y &rest args)
"Args: (x y &rest args)
Returns a Gamma regression model. Accepts :LINK, :OFFSET and :VERBOSE
keywords in addition to the keywords accepted by regression-model."
  (apply #'send gammareg-proto :new :x x :y y args))

;;;;
;;;;
;;;;                Some Simple Design Matrix Tools
;;;;
;;;;

(defun indicators (x &key (drop-first t) (test #'eql))
"Args: (x &key (drop-first t) (test #'eql))
Returns a list of indicators sequences for the levels of X. TEST is
used to check equality of levels. If DROP-FIRST is true, the indicator
for the first level is dropped."
  (let ((levels (remove-duplicates (coerce x 'list))))
    (mapcar #'(lambda (lev) (if-else (map-elements test lev x) 1 0))
            (if drop-first (rest levels) levels))))

(defun cross-terms (x &rest args)
"Args: (x &rest args)
Arguments should be lists. Returns list of cross products, with the first
argument list varying slowest."
  (case (length args)
    (0 (error "too few arguments"))
    (1 (let ((y (first args)))
         (apply #'append 
                (mapcar #'(lambda (a) (mapcar #'(lambda (b) (* a b)) y)) x))))
    (t (cross-terms x (apply #'cross-terms args)))))

(defun level-names (x &key (prefix "") (drop-first t))
"Args: (x &key (prefix \"\") (drop-first t))
Constructs name strings using unique levels in X and PREFIX."
  (let ((levels (remove-duplicates (coerce x 'list))))
    (mapcar #'(lambda (x) (format nil "~a(~a)" prefix x))
            (if drop-first (rest levels) levels))))

(defun cross-names (x &rest args)
"Args: (x &rest args)
Arguments should be lists. Constructs cross products of names, separated
by dots. First index varies slowest."
  (flet ((paste (x y) (format nil "~a.~a" x y)))
    (case (length args)
      (0 (error "too few arguments"))
      (1 (let ((y (first args)))
           (apply #'append
                  (mapcar #'(lambda (a) (mapcar #'(lambda (b) (paste a b)) y))
                          x))))
      (t (cross-names x (apply #'cross-names args))))))

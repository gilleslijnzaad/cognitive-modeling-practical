;;  Sustained Attention to Response Task (SART)
;;
;;  In each trial the participant sees a letter: "O" or "Q".
;;  They must press a key every time an O appears (90% of trials),
;;  but withhold their response when the stimulus is a Q (10% of trials).
;;
;;  Cognitive Modelling Practical 2021
;;  Updated for ACT-R 7.21 by Loran Knol


;;===================;;
;;  Experiment code  ;;
;;===================;;


;; Experiment settings
(defvar *stimulus-duration* 2) ; number of seconds the stimulus is shown
(defvar *inter-stimulus-interval* 0.5) ; number of seconds between trials
(defvar *target-trials* 180) ; number of target trials
(defvar *non-target-trials* 20) ; number of non-target trials

(defvar *output-directory* "~/output/") ; location where output files are stored
(defvar *trace-to-file-only* t) ; whether the model trace should only be saved to file and not appear in terminal
(defvar *trace-file-name* "sart-trace-high") ; name of file in which the trace is stored

(defvar *terminal-stream* *standard-output*) ; necessary for stream management

(defvar *visible* nil) ; visibility of the experiment window

;; Global variables for data storage
(defvar *stimuli* nil)
(defvar *trial-response* nil)
(defvar *trial-start* nil)
(defvar *trial-rt* nil)
(defvar *trial-done* nil)
(defvar *all-responses* nil)
(defvar *all-rts* nil)

;; Parallellism management
(defvar *lock* (bt:make-lock))

;; Do SART experiment n times, save trace to output file
(defun do-sart-n (n)
  (with-open-file
    (*file-stream*
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name *trace-file-name* :type "txt")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)

  (if *trace-to-file-only* 
    ; If true, direct standard output only file
    (setf *standard-output* *file-stream*)
    ; Else, direct standard output to terminal and file
    (setf *standard-output*
      (make-broadcast-stream *terminal-stream* *file-stream*)))

  ; Direct ACT-R output to the stream contained within *standard-output*
  (echo-act-r-output)

  (setf *visible* nil)
  (format t "Running ~a model participants~%" n)
  (dotimes (i n)
    (let ((participant (1+ i)))
      (format t "Run ~a...~%" participant)
      (do-sart)
      (write-results-to-file
        (concatenate 'string "dat" (write-to-string participant))
        participant
        *stimuli*
        (reverse *all-responses*)
        (reverse *all-rts*))))
  (format t "Done~%")

  ; We will close *file-stream* now, so make sure *standard-output*
  ; no longer points to it
  (setf *standard-output* *terminal-stream*)
  ; We also have to make sure ACT-R knows about the new value of
  ; *standard-output*
  (echo-act-r-output)
  )
)

;; Do SART experiment 1 time
(defun do-sart ()
  (reset)
  (setf *all-responses* nil)
  (setf *all-rts* nil)
  (setf *stimuli*
    (permute-list
      (concatenate
        'list
        (make-array *target-trials* :initial-element "O")
        (make-array *non-target-trials* :initial-element "Q"))))
  (setf *visible* nil)
  (loop for stim in *stimuli* do (run-trial stim))
)

;; Do a single SART trial with a target stimulus
(defun do-sart-trial-o ()
  (setf *visible* t)
  (run-trial "O")
)

;; Do a single SART trial with a non-target stimulus
(defun do-sart-trial-q ()
  (setf *visible* t)
  (run-trial "Q")
)


;; Execute a trial with a given stimulus
(defun run-trial (stim)
  (let ((window (open-exp-window "SART Experiment"
                               :visible *visible*
                               :width 300
                               :height 300
                               :x 300
                               :y 300)))

    (add-text-to-exp-window window
                            stim
                            :width 30
                            :height 30
                            :x 145
                            :y 150)

  (add-act-r-command
    "sart-key-press"
    'key-event-handler
    "SART task key press monitor")
  (monitor-act-r-command "output-key" "sart-key-press")

  (setf *trial-response* nil)
  (setf *trial-start* (get-time))
  (setf *trial-rt* nil)
  (setf *trial-done* nil)

  (install-device window)
  (run-full-time *stimulus-duration* *visible*)
  (clear-exp-window)
  (run-full-time *inter-stimulus-interval* *visible*)

  (remove-act-r-command-monitor "output-key" "sart-key-press")
  (remove-act-r-command "sart-key-press"))

  ; Prevent race conditions
  (bt:with-lock-held
    (*lock*)
    (push *trial-response* *all-responses*)
    (push *trial-rt* *all-rts*))
)

;; Register the model's key presses (ignore the model parameter)
(defun key-event-handler (model key)
  (declare (ignore model))

  ; Prevent race conditions
  (bt:with-lock-held
  (*lock*)
    (setf *trial-rt* (/ (- (get-time) *trial-start*) 1000.0))
    (setf *trial-response* (string key))
    (setf *trial-done* t))
)

;; Write the behavioural results to a file
(defun write-results-to-file (name participant stimuli responses rts)
  (with-open-file
    (out
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name name :type "csv")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format out "participant, trial, stimulus, response, rt~%")
    (loop
      for trial from 1
      for stimulus in stimuli
      for response in responses
      for rt in rts
      do (format out "~a, ~a, ~a, ~a, ~a~%" participant trial stimulus response rt)))
)



;;===================;;
;;    Model code     ;;
;;===================;;

(clear-all)

(define-model sart

;; Model parameters
(sgp :v t ; main trace detail
  :act low ; activation trace detail
  :sact t ; include activation trace in main trace

  :show-focus t ; show where the model is looking
  :esc t ; enable sub-symbolic level
  :rt -5 ; retrieval threshold
  :bll 0.5 ; base-level learning
  :ans 0.2 ;activation noise
)

(chunk-type beginning label)
(chunk-type goal state)
(chunk-type subgoal step)
(chunk-type srmapping stimulus hand)
(chunk-type previous stimulus)

(add-dm
  (start isa chunk)
  (press-on-O isa srmapping stimulus "O" hand left)
  (withhold-on-Q isa srmapping stimulus "Q" hand nil)
  (O isa previous stimulus "O")
  (Q isa previous stimulus "Q")
  (startgoal isa beginning label start)
  (attend isa goal state attend)
  (wander isa goal state wander)
  (identify isa subgoal step identify)
  (get-response isa subgoal step get-response)
  (make-response isa subgoal step make-response)
  (save-stimulus isa subgoal step save-stimulus)
)

(set-base-levels
  (attend      10000  -10000)
  (wander      10000  -10000)
  (press-on-O    10000  -10000)
  (withhold-on-Q  10000  -10000)
)

(p start-task
  =goal>
    isa      beginning
    label    start
  ?retrieval>
    buffer   empty
    state    free
  - state    error
==>
  +retrieval>
    isa      goal
    state    attend
  -goal>
)

(p check-current-goal
  =retrieval>
    isa           goal
  ?retrieval>
    state         free
  - state         error
  ?goal>
    buffer        empty
  ?visual>
  - scene-change  T
==>
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa           goal
  - state         nil
)

(p respond-wandering
  ?goal>
    buffer      empty
  =retrieval>
    isa         goal
    state       wander
  =visual-location>
  ?manual>
    state     free
==>
  +manual>
    isa       punch
    hand      left
    finger    index
  -goal>
  -visual-location>
  -visual>
  +retrieval>
    isa       goal
  - state     nil
)

(p detect-first-stimulus
  ?goal>
    buffer      empty
  =retrieval>
    isa         goal
    state       attend
  =visual-location> ;something has appeared on the screen
  ?visual>
    state       free
    buffer      empty
==>
  =retrieval>
    state       nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +visual>
    isa         move-attention
    screen-pos  =visual-location
  +goal>
    isa         subgoal
    step        save-stimulus
)

(p detect-stimulus
  ;haal uit imaginal buffer
  ;retrieve response
  ?goal>
    buffer      empty
  =retrieval>
    isa         goal
    state       attend
  =visual-location> ;something has appeared on the screen
  =imaginal>
    stimulus    =stim
  ?visual>
    state       free
==>
  =retrieval>
    state       nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval> ;makes retrieval request for stimulus in imaginal
    isa         srmapping
    stimulus    =stim
  +goal>
    isa         subgoal
    step        make-response
  -imaginal>
  +visual>
    isa         move-attention
    screen-pos  =visual-location
)

;respond

(p respond-if-O
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    stimulus  "O"
    hand      =hand
  ?manual>
    state     free
==>
  +manual>
    isa       punch
    hand      =hand
    finger    index
  +goal>
    isa       subgoal
    step      save-stimulus
)

(p do-not-respond-if-Q
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    stimulus  "Q"
    hand      nil
  ==>
  +goal>
    isa       subgoal
    step      save-stimulus
)

(p save-imaginal
  ;get information from imaginal
  =goal>
    isa       subgoal
    step      save-stimulus
  =visual>
    isa       text
    value     =letter
  ?visual>
    state     free
  ?retrieval>
    state     free
  ?imaginal>
    buffer    empty
    state     free 
==>
  -goal>
  +visual>
    isa       clear-scene-change
  +imaginal>
    isa       previous
    stimulus  =letter  
  +retrieval>
    isa       goal
  - state     nil
  -visual>
  -visual-location>
)


(goal-focus startgoal)

)
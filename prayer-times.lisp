(defpackage #:prayer-times
  (:use :cl :alexandria))

(in-package #:prayer-times)

(defvar conf-dir (uiop:native-namestring #p"~/.config/prayer-times/"))
(defvar remind-before "00:05:00")

(defvar latitude 30.983334)
(defvar longitude 41.016666)
(defvar school 0)
(defvar meth 4)

(defvar year (local-time:timestamp-year (local-time:now)))
(defvar month (local-time:timestamp-month (local-time:now)))
(defvar day (local-time:timestamp-day (local-time:now)))

(defvar api "https://api.aladhan.com/v1/calendar")

(defvar tasbih-file (merge-pathnames #p"tasbih.mp3" conf-dir))
(defvar adhan-file (merge-pathnames #p"adhan.mp3" conf-dir))

(defvar tasbih nil)
(defvar adhan nil)
(defvar out nil)

(defun initialize-sound ()
  (setf tasbih (cl-mpg123:make-file tasbih-file))
  (setf adhan (cl-mpg123:make-file adhan-file))

  (cl-mpg123:connect tasbih)
  (cl-mpg123:connect adhan)

  (setf out (make-instance 'cl-out123:output))
  (cl-out123:connect out :driver "alsa")
  (cl-out123:start out))

(defun play-sound (file)
  (loop with buffer = (cl-mpg123:buffer file)
        for bytes = (cl-mpg123:process file)
        until (= 0 bytes)
        do (cl-out123:play out buffer bytes))
  (cl-mpg123:seek file 0 :mode :absolute))

(defun prayer-to-arabic (p)
  (switch (p :test #'equal)
    ("Fajr" "الفجر")
    ("Dhuhr" "الظهر")
    ("Asr" "العصر")
    ("Maghrib" "المغرب")
    ("Isha" "العشاء")))

(defun dl-json ()
  (ensure-directories-exist conf-dir)
  (dotimes (m 12)
    (let ((file (format nil "~a~a.json" conf-dir (1+ m))))
      (unless (probe-file file)
        (with-open-file (str file
                             :direction :output
                             :if-exists :supersede)
          (format str "~a" (dex:get (format nil "~a/~a/~a?latitude=~a&longitude=~a&method=~a&school=~a"
                                            api year (1+ m) latitude longitude meth school))))))))

(defun 12-hour (n)
  "Convert 24 hour format to 12 hour format"
  (if (> n 12)
      (- n 12)
      n))

(defun prayers-list (ts)
  (with-open-file (str (format nil "~a~a.json" conf-dir month)
                       :direction :input)
    (let* ((y (local-time:timestamp-year ts))
           (m (local-time:timestamp-month ts))
           (d (local-time:timestamp-day ts))
           (timings (gethash "timings" (nth (1- d) (gethash "data" (yason:parse str)))))
           (acc nil))
      (dolist (p '("Fajr" "Dhuhr" "Asr" "Maghrib" "Isha"))
        (ppcre:register-groups-bind (hour minute timezone)
            ("^([0-9]+):([0-9]+) \\(([-\\+][0-9]+)\\)" (gethash p timings))
          (push (cons (prayer-to-arabic p)
                      (local-time:parse-timestring
                       (format nil "~a-~a-~aT~a:~a:00~a"
                               y m d hour minute timezone)))
                acc)))
      (nreverse acc))))

(defun next-prayer ()
  (let ((now (local-time:now)))
    (dolist (p (prayers-list now))
      (let ((pts (timestamp-to-prayer p)))
        (if pts (return-from next-prayer pts))))
    ;; next day Fajr
    (timestamp-to-prayer
     (car (prayers-list
           (local-time:timestamp+ now 1 :day))))))

(defun timestamp-to-prayer (p)
  (let ((n (car p))
        (ti (cdr p)))
    (if (local-time:timestamp<= (local-time:now) ti)
        (cons n (format-time (time-diff ti (local-time:now)))))))

(defun time-diff (x y)
  (local-time:universal-to-timestamp
   (truncate (local-time:timestamp-difference x y))))

;; TODO: make it a macro if we want to list times
(defun format-time (ti &optional local-time-zone-p)
  (local-time:with-decoded-timestamp (:sec sec :minute min :hour hour :timezone local-time:+utc-zone+) ti
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun format-yambar (p)
  (format t "name|string|~a~%time|string|~a~%~%"
          (car p) (cdr p)))

;; notifications

(defun notify-send (title text)
  (uiop:run-program (format nil "notify-send \"~a\" \"~a\"" title text)))

;; -------------

;; TODO (PERFORMANCE): maybe test equality on the last char (second)?
(defun main ()
  (dl-json)
  (initialize-sound)
  (format-yambar (next-prayer))
  (loop with prev = (cdr (next-prayer))
        do (let* ((np (next-prayer))
                  (p (car np))
                  (ti (cdr np)))
             (when (string-not-equal ti prev)
               (setf prev ti)
               (format-yambar (next-prayer))

               ;; remind before
               (when (string-equal ti remind-before)
                 (notify-send (format nil "~a بعد قليل" p)
                              (format nil "تبقى ~a حتى أذان ~a" ti p))
                 (bt:make-thread (lambda ()
                                   (play-sound tasbih))))
               
               (when (string-equal ti "00:00:00")
                 (notify-send (format nil "حان الآن وقت أذان ~a" p)
                              (format nil "حان الآن وقت أذان ~a" p))
                 (bt:make-thread (lambda ()
                                   (play-sound adhan))))))))

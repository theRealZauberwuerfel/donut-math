(deftype fuzzy-float () '(float 0.0 1.0))

(defconstant +theta-spacing+ (the fuzzy-float 0.07))
(defconstant +phi-spacing+   (the fuzzy-float 0.02))

(defconstant +R1+ (the fuzzy-float 1.0))
(defconstant +R2+ (the float 2.0))
(defconstant +K2+ (the float 5.0))

(defconstant +K1+ (the float (/ (* *screen-width* +K2+ 3)
                                (* 8 (+ +R1+ +R2+)))))

(defun render-frame (A B)
  (declare (type float A B))
  (let ((cos[A] (cos A)) (sin[A] (sin A))
        (cos[B] (cos B)) (sin[B] (sin B)))
    (let ((output (make-array '(#.*screen-width* #.*screen-height*)
                              :element-type 'character
                              :initial-element #\Space))
          (zbuffer (make-array '(#.*screen-width* #.*screen-height*)
                               :element-type 'fixnum
                               :initial-element 0))
          ;; literally 2*PI
          (tau (the float 6.283185)))
      (loop
        :named outer
        :for theta :from 0.0 :by +theta-spacing+
        :while (< theta tau)
        :do (let ((cos[theta] (cos theta))
                  (sin[theta] (sin theta)))
              (loop
                :named inner
                :for phi :from 0.0 :by +phi-spacing+
                :while (< phi tau)
                :do (let ((cos[phi] (cos phi))
                          (sin[phi] (sin phi)))
                      (let ((circle-x (+ +R2+ (* +R1+ cos[theta])))
                            (circle-y (* +R1+ sin[theta])))
                        (let* ((x (- (* circle-x (+ (* cos[B] cos[phi])
                                                    (* sin[A] sin[B] sin[phi])))
                                     (* circle-y cos[A] sin[B])))
                               (y (+ (* circle-x (- (* sin[B] cos[phi])
                                                    (* sin[A] cos[B] sin[phi])))
                                     (* circle-y cos[A] cos[B])))
                               (z (+ +K2+ (* cos[A] circle-x sin[phi]) (* circle-y sin[A])))
                               (1/z (/ z)))
                          (let ((x-proj (+ (/ *screen-width* 2)
                                           (* +K1+ 1/z x)))
                                (y-proj (- (/ *screen-height* 2)
                                           (* +K1+ 1/z y))))
                            (let ((L (+ (* cos[phi] cos[theta] sin[B])
                                        (- (* cos[A] cos[theta] sin[phi]))
                                        (- (* sin[A] sin[theta]))
                                        (* cos[B] (- (* cos[A] sin[theta])
                                                     (* cos[theta] sin[A] sin[phi]))))))
                              (when (> L 0)
                                (when (> 1/z (aref zbuffer x-proj y-proj))
                                  (setf (aref zbuffer x-proj y-proj) 1/z)
                                  (let ((luminance-index (* L 8)))
                                    (setf (aref output x-proj y-proj)
                                          (svref ".,-~:;=!*#$@" luminance-index)))))))))))))
      (format t "\x1b[H")
      (loop
        :for j :from 0 :by 1
        :while (< j *screen-height*)
        :do (loop
              :for i :from 0 :by 1
              :while (< i *screen-width*)
              :do (format t "~c" (aref output i j)))
            (format t "~%")))))

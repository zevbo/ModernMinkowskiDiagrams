#lang racket
(require (except-in plot line))
(require 2htdp/universe)

(define AXIS_WIDTH 3)
(define POINT_RADIUS 6)

;; These are just structs that store data about either an axis, or a point
(struct axis (max-ct min-ct max-x min-x vel))
(struct point (ct x vel) #:transparent)

;; Getting the angle just by using atan
(define (v->angle v) (radians->degrees (v->angle-rad v)))
(define (v->angle-rad v) (atan v))

;; Gets the sacle of an axis or velocity
(define (vel->scale v)
  (sqrt (/ (+ 1 (expt v 2)) (- 1 (expt v 2)))))
(define (get-scale axis) (vel->scale (axis-vel axis)))

;; Creates a ct or x axis for a given set of axis. If given a non-0 x or ct, it becomes a girdline
(define (ct-axis a
                 #:color[color "black"]
                 #:width[width AXIS_WIDTH]
                 #:x[x 0])
  (define angle (- (/ pi 2) (v->angle-rad (axis-vel a))))
  (define other-angle (- (/ pi 2) angle))
  (define scale (get-scale a))
  (define h-shift (* (cos other-angle) x scale))
  (define v-shift (* (sin other-angle) x scale))
  (lines
   (list
     (vector (+ (* (axis-min-ct a) (cos angle) scale) h-shift)
             (+ (* (axis-min-ct a) (sin angle) scale) v-shift))
     (vector (+ (* (axis-max-ct a) (cos angle) scale) h-shift)
             (+ (* (axis-max-ct a) (sin angle) scale) v-shift)))
   #:color color #:width width))

(define (x-axis a
                 #:color[color "black"]
                 #:width[width AXIS_WIDTH]
                 #:ct[ct 0])
  (define angle (v->angle-rad (axis-vel a)))
  (define other-angle (- (/ pi 2) angle))
  (define scale (get-scale a))
  (define h-shift (* (cos other-angle) ct scale))
  (define v-shift (* (sin other-angle) ct scale))
  (lines
   (list
    (vector (+ (* (axis-min-x a) (cos angle) scale) h-shift)
            (+ (* (axis-min-x a) (sin angle) scale) v-shift))
    (vector (+ (* (axis-max-x a) (cos angle) scale) h-shift)
            (+ (* (axis-max-x a) (sin angle) scale) v-shift)))
   #:color color #:width width))

(define BACKGROUND_SCALE 1.7)

;; Creates the x-axis and ct-axis for a set of axis
(define (axis-lines axis
              #:color[color "black"]
              #:width[width AXIS_WIDTH])
   (list
    (ct-axis axis #:color color #:width width)
    (x-axis  axis #:color color #:width width)))

(define GRIDLINE_WIDTH 1)

;; Given a point in one velocity, uses lawrence transformations to see the coordinates in another frame
(define (change-vel p new-v)
  (define del-v (- new-v (point-vel p)))
  (define gamma (/ 1 (sqrt (- 1 (expt del-v 2)))))
  (define x  (* gamma (- (point-x  p) (* (point-ct p) del-v))))
  (define ct (* gamma (- (point-ct p) (* (point-x  p) del-v))))
  (point ct x new-v))

;; Given the data of a point, creates the info for graphing it
(define (make-point point
                   #:color[color "red"] #:size[point-radius POINT_RADIUS])
  (set! point (change-vel point 0))
  (points
   (list (vector (point-x point) (point-ct point)))
   #:color color #:line-width point-radius #:size point-radius))

;; Creates the info to graph an axis, with a gridline at every single integer x and ct
(define (axis-w/gridlines axis #:color[color "black"])
  (define lines (axis-lines axis))
  (define angle (- 0 (v->angle-rad (axis-vel axis))))
  (define ct-gridlines
    (map
     (lambda (val)
       (ct-axis axis #:x val #:color color #:width GRIDLINE_WIDTH))
     (range (+ (axis-min-x axis) 1) (axis-max-x axis))))
  (define x-gridlines
    (map
     (lambda (val)
       (x-axis axis #:ct val #:color color #:width GRIDLINE_WIDTH))
     (range (+ (axis-min-x axis) 1) (axis-max-x axis))))
  (list lines ct-gridlines x-gridlines))

;; Creates a light cone, that by default comes from the center but will come from any point it is given
(define (create-light-cone a #:point[p (point 0 0 0)])
  (change-vel p 0)
  (define x  (point-x  p))
  (define ct (point-ct p))
  (define bottom-left  (max (- (axis-min-ct a) ct) (- (axis-min-x a) x)))
  (define top-right    (min (- (axis-max-ct a) ct) (- (axis-max-x a) x)))
  (define bottom-right (max (- (axis-min-ct a) ct) (- x (axis-max-x a))))
  (define top-left     (min (- (axis-max-ct a) ct) (- x (axis-min-x a))))
  (list
   (lines (list (vector (+ bottom-left        x) (+ bottom-left  ct))
                (vector (+ top-right          x) (+ top-right    ct))))
   (lines (list (vector (+ (- 0 bottom-right) x) (+ bottom-right ct))
                (vector (+ (- 0 top-left)     x) (+ top-left     ct))))))

(define v 0)
(define size 4)
(define s1-gridlines? #f)
(define s2-gridlines? #f)
(define light-cone? #f)
(define px 0)
(define py 0)

(define (handle-pad-events x k)
  (case (string->symbol k)
    [(right)  (set! v (min (+ v 0.01)  0.99))]
    [(left)   (set! v (max (- v 0.01) -0.99))]
    [(up)     (set! size (+ size 1))]
    [(down)   (set! size (max (- size 1) 2))]
    [(rshift)    (set! s2-gridlines? (not s2-gridlines?))]
    [(shift) (set! s1-gridlines? (not s1-gridlines?))]
    [(| |)      (set! light-cone?   (not light-cone?))]
    [(w) (set! py (+ py 1))]
    [(s) (set! py (- py 1))]
    [(d) (set! px (+ px 1))]
    [(a) (set! px (- px 1))]
    ))

(define last-image 0)

;; This is called every tick to generate the gui
(define (create-image _)
  (define big-size (* 3 size))
  (define a1 (axis big-size (- 0 big-size) big-size (- 0 big-size) 0))
  (define a2 (axis big-size (- 0 big-size) big-size (- 0 big-size) v))
  (define ai1 ((if s1-gridlines? axis-w/gridlines axis-lines) a1))
  (define ai2 ((if s2-gridlines? axis-w/gridlines axis-lines) a2))
  (define p (point py px 0))
  (define pi (make-point p))
  (define light-cone (if light-cone? (create-light-cone a1 #:point p) (list)))
  (define i
    (plot (list ai1 ai2 light-cone pi)
        #:x-min (- 0 size) #:x-max size #:y-min (- 0 size) #:y-max size
        #:height 750 #:width 750))
  (set! last-image i)
  i)

(big-bang (lambda (_) 0)
    [to-draw create-image]
    [on-pad handle-pad-events])

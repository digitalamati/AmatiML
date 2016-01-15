#lang racket
(require racket/draw)

(module+ test
         (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(provide drawp)
(define drawp #t)

(provide arccolor)
(define (arccolor c)
  (set! arc-color c)
  'arc-color-set)

(define circlecolor "red")

(define arc-color "blue")

(provide elaboration)
(define (elaboration b)
  (set! drawp b)
  'elaboration-done)

(provide mirrorp)
(define mirrorp #f)

(provide mirroring)
(define (mirroring b)
  (set! mirrorp b)
  'mirroring-done)

(define tracing #f)

(provide edge-tracing)
(define (edge-tracing b o)
  (set! tracing b)
  (set! overhang o)
  'tracing-done)

(provide cello-overhang)
(define cello-overhang 5)

(provide violin-overhang)
(define violin-overhang 3.5)

(define overhang cello-overhang)

(define dthreshold -0.75)

(provide determinant-threshold)
(define (determinant-threshold d)
  (set! dthreshold d)
  'determinant-threshold-done)

(define drawing-title "")
(define coder "")

(provide title)
(define (title m)
  (set! drawing-title m)
  'title-done)

(provide coded-by)
(define (coded-by m)
  (set! coder m)
  'codedby-done)

(provide first)
(define first car)

(provide second)
(define second cadr)

; major part M of sections S+M=1

(provide harmonic)
(define harmonic (/ 1 (+ 1 (/ 1 (sqrt 2)))))

(provide goldenmean)
(define goldenmean (/ (+ 1 (sqrt 5)) 2))

(provide geometric)
(define geometric (- goldenmean 1))

(provide arithmetic)
(define arithmetic (/ 2 3))

(provide subharmonic)
(define subharmonic (- 1 (/ (sqrt 2) 2)))



(provide square)
(define (square x) (* x x))

(provide sign)
(define (sign x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (else 0)))

(provide :)
(define (: x y)
  (let ((s (sign (* x y)))
        (ax (abs x))
        (ay (abs y)))
    (* s (/ ax (+ ax ay)))))

(define (d+ x) (+ 0.5 x))
(define (d- x) (- x 0.5))

(provide average)
(define (average x y) (/ (+ x y) 2))

(define (index n)
  (if (= n 0)
    (list 0)
    (cons n (index (- n 1)))))

(provide ints)
(define (ints from to)
  (if (> from to)
    '()
    (cons from (ints (+ 1 from) to))))

(provide scaled)
(define (scaled s lst)
  (map (lambda (x) (* x s)) lst))

(define (begins-with? d tag)
  (if (list? d)
    (eq? (car d) tag)
    #f))

(provide minus)
(define (minus x) (- 0 x))

;; Primitive error handler

(define callerror 
  (lambda m
    (write m)
    (newline)
    (error 'error!)))

;; Points

(provide point)
(define (point x y)
  (list 'point "" x y))

(provide origin)
(define origin (point 0 0))

(provide point?)
(define (point? p)
  (begins-with? p 'point))

(define (labelof p)
  (if (point? p)
    (cadr p)
    (callerror '(labelof: not a point) p)))

(provide xcor)
(define (xcor p)
  (if (point? p)
    (caddr p)
    (callerror '(xcor: not a point) p)))

(provide ycor)
(define (ycor p)
  (if (point? p)
    (cadddr p)
    (callerror '(ycor: not a point) p)))

(provide xshift)
(define (xshift p d)
  (point (+ (xcor p) d) (ycor p)))

(provide yshift)
(define (yshift p d)
  (point (xcor p) (+ (ycor p) d)))

(provide mirror)
(define (mirror p)
  (point (- (xcor p)) (ycor p)))

(provide mirrorcircle)
(define (mirrorcircle c) (circle (mirror (center c)) (radius c)))

(provide transpose)
(define (transpose obj)
  (if (point? obj)
    (point (ycor obj) (xcor obj))
    (if (line? obj)
      (line (transpose (cadr obj)) 
            (transpose (caddr obj)))
      (if (circle? obj)
        (circle (transpose (cadr obj))
                (caddr obj))
        (callerror '(transpose: what?) obj)))))

(provide xdistance)
(define (xdistance p1 p2)
  (abs (- (xcor p1) (xcor p2))))

(provide ydistance)
(define (ydistance p1 p2)
  (abs (- (ycor p1) (ycor p2))))

(provide xsquish)
(define (xsquish pt p)
  (point (* p (xcor pt)) (ycor pt)))

(provide ysquish)
(define (ysquish pt p)
  (point (xcor pt) (* p (ycor pt))))

; comparing a list of two points

(provide left)
(define (left twopoints)
  ((if (< (xcor (car twopoints))
          (xcor (cadr twopoints)))
     car
     cadr)
   twopoints))

(provide right)
(define (right twopoints)
  ((if (< (xcor (car twopoints))
          (xcor (cadr twopoints)))
     cadr
     car)
   twopoints))

(provide bottom)
(define (bottom twopoints)
  ((if (< (ycor (car twopoints))
          (ycor (cadr twopoints)))
     car
     cadr)
   twopoints))

(provide top)
(define (top twopoints)
  ((if (< (ycor (car twopoints))
          (ycor (cadr twopoints)))
     cadr
     car)
   twopoints))

; distance

(provide distance)
(define (distance p1 p2)
  (let ((x1 (xcor p1))
        (y1 (ycor p1))
        (x2 (xcor p2))
        (y2 (ycor p2)))
    (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))

;; Vectors

(provide vec)
(define (vec p q)
  (point (- (xcor q) (xcor p)) (- (ycor q) (ycor p))))

(provide vec+)
(define (vec+ v1 v2)
  (point (+ (xcor v1) (xcor v2)) (+ (ycor v1) (ycor v2))))

(provide vecneg)
(define (vecneg v)
  (point (- (xcor v)) (- (ycor v))))

(provide vec-)
(define (vec- v1 v2)
  (vec+ v1 (vecneg v2)))

(provide scalevec)
(define (scalevec s v)
  (point (* s (xcor v)) (* s (ycor v))))

;; Lines

(provide line)
(define (line p1 p2)
  (if (and (point? p1) (point? p2))
    (list 'line p1 p2)
    (callerror '(line: not both points) p1 p2)))

(define (line? L)
  (begins-with? L 'line))

(provide first-point)
(define (first-point L)
  (if (line? L)
    (cadr L)
    (callerror '(first-point: not a line) L)))

(provide second-point)
(define (second-point L)
  (if (line? L)
    (caddr L)
    (callerror '(second-point: not a line) L)))

(provide mirrorline)
(define (mirrorline L)
  (line (mirror (first-point L)) (mirror (second-point L))))

(provide slope)
(define (slope L)
  (if (line? L)
    (let ((x0 (xcor (first-point L)))
          (y0 (ycor (first-point L)))
          (x1 (xcor (second-point L)))
          (y1 (ycor (second-point L))))
      (let ((xdiff (- x1 x0)))
        (if (= xdiff 0)
          'infinity
          (let ((m (/ (- y1 y0) xdiff)))
            (if (> (abs m) 10000)
              'infinity
              m)))))
    (callerror '(slope: not a line) L)))

(define (offset L)
  (if (line? L)
    (let ((m (slope L)))
      (if (eq? m 'infinity)
        '(offset error)
        (let ((x0 (xcor (first-point L)))
              (y0 (ycor (first-point L))))
          (- y0 (* m x0)))))
    (callerror '(offset: not a line) L)))

(define (make-line-slope-offset slope offset)
  (line (point 0 offset) (point 1 (+ offset slope))))

(provide make-line)
(define (make-line slope pt)
  (line pt (point (+ (xcor pt) 100) (+ (ycor pt) (* slope 100)))))

(provide linefun)
(define (linefun l)
  (let ((m (slope l))
        (b (offset l)))
    (lambda (x) (+ b (* m x)))))

(provide linefrom)
(define (linefrom m pt)
  (line pt (point (+ (xcor pt) 100) (+ (ycor pt) (* m 100)))))

(provide funline)
(define (funline f)
  (line (point 0 (f 0)) (point 100 (f 100))))

(define (vertical? L)
  (not (number? (slope L))))

(define (horizontal? L)
  (let ((s (slope L)))
    (if (number? s)
      (< (abs s) .001)
      #f)))

(provide x-axis)
(define x-axis (line origin (point 100 0)))

(provide y-axis)
(define y-axis (line (point 0 100) origin))

(provide horizontal)
(define (horizontal p) (line p (point (+ (xcor p) 100) (ycor p))))

(provide vertical)
(define (vertical p) (line p (point (xcor p) (+ (ycor p) 100))))

(provide at)
;(define (at V H) (intersect (vertical V) (horizontal H)))
(define (at p q) (point (xcor p) (ycor q)))

;; Circles

(define (distance? d)
  (if (number? d)
    (>= d 0)
    #f))

(provide circle)
(define (circle c d)
  (if (and (point? c) (distance? d))
    (list 'circle c d)
    (callerror '(circle: not a point or a distance) c d)))

(provide circlefrom)
(define (circlefrom p q) (circle p (distance p q)))

(define (circle? C)
  (begins-with? C 'circle))

(provide center)
(define (center circle)
  (if (circle? circle)
    (cadr circle)
    (callerror '(center: not a circle) circle)))

(provide radius)
(define (radius circle)
  (if (circle? circle)
    (caddr circle)
    (callerror '(radius: not a circle) circle)))

(provide csquish)
(define (csquish c p)
  (circle (center c) (* (radius c) p)))

(provide right-circle)
(define (right-circle circles)
  (let ((circle1 (car circles))
        (circle2 (cadr circles)))
    (if (> (xcor (center circle1))
           (xcor (center circle2)))
      circle1
      circle2)))

(provide left-circle)
(define (left-circle circles)
  (let ((circle1 (car circles))
        (circle2 (cadr circles)))
    (if (< (xcor (center circle1))
           (xcor (center circle2)))
      circle1
      circle2)))

(provide upper-circle)
(define (upper-circle circles)
  (let ((circle1 (car circles))
        (circle2 (cadr circles)))
    (if (> (ycor (center circle1))
           (ycor (center circle2)))
      circle1
      circle2)))

(provide lower-circle)
(define (lower-circle circles)
  (let ((circle1 (car circles))
        (circle2 (cadr circles)))
    (if (> (ycor (center circle1))
           (ycor (center circle2)))
      circle2
      circle1)))

(provide north)
(define (north c) (yshift (center c) (radius c)))

(provide south)
(define (south c) (yshift (center c) (- (radius c))))

(provide east)
(define (east c) (xshift (center c) (radius c)))

(provide west)
(define (west c) (xshift (center c) (- (radius c))))

(provide circlethrough)
(define (circlethrough p q r)
  ; circle through points p, q, r
  (let* ((pq (bisector p q))
         (pr (bisector p r))
         (c (intersect pq pr)))
    (circlefrom c p)))

;; Intersections

(define (intersect-line-line line1 line2)
  (if (and (vertical? line2) (vertical? line1))
    (callerror '(intersect-line-line: both vertical) line1 line2)
    (if (vertical? line1)
      (intersect-line-line line2 line1) 
      ;; Now first line is not vertical
      (if (vertical? line2)
        (let ((m (slope line1))
              (b (offset line1))
              (x (xcor (first-point line2))))
          (point x (+ (* m x) b)))
        (let ((m1 (slope line1))
              (m2 (slope line2))
              (b1 (offset line1))
              (b2 (offset line2)))                    
          (if (and (= m1 m2) (= b1 b2))
            (callerror '(intersect-line-line: identical lines) line1 line2)
            (point (/ (- b2 b1) (- m1 m2))
                   (/ (- (* m2 b1) (* m1 b2))
                      (- m2 m1)))))))))

;; for roundoff errors...

(define (correct r)
  (if (< (abs (- r (round r))) .1) (round r) r))

;(define dthreshold -0.75)

(define (quadsolve a b c) ;; solutions to ax^2+bx+c = 0
  (let ((d (correct (- (* b b) (* 4 a c)))))
    (if (< d dthreshold)
      (callerror '(quadsolve: no solutions) a b c d)
      (let ((negb (- 0 b))
            (d (if (< d 0) 0 d)))
        (if (= d 0)
          (list (/ negb (* 2 a)))
          (let ((sqrtd (sqrt d))
                (twoa (* 2 a)))
            (list (/ (+ negb sqrtd) twoa)
                  (/ (- negb sqrtd) twoa))))))))           

(define (intersect-circle-line c l)
  (let ((x0 (xcor (center c)))
        (y0 (ycor (center c)))
        (d (radius c))
        (x1 (xcor (first-point l)))
        (y1 (ycor (first-point l)))
        (x2 (xcor (second-point l)))
        (y2 (ycor (second-point l))))
    (if (vertical? l)
      (let ((solns
              (intersect-circle-line
                (circle (point y0 x0) d)
                (line (point y1 x1) (point y2 x2)))))
        (if (point? solns) (transpose solns) (map transpose solns)))
      (let ((m (slope l))
            (b (offset l)))
        (let ((aa (+ (square m) 1))
              (bb (* 2 (+ (* b m)
                          (- (* y0 m))
                          (- x0))))
              (cc (+ (square b)
                     (square x0)
                     (- (* 2 y0 b))
                     (square y0)
                     (- (square d)))))
          (let ((solns (map (lambda (x) (point x (+ (* m x) b)))
                            (quadsolve aa bb cc))))
            (if (null? solns) 
              '()
              (if (= (length solns) 1)
                solns
                (let ((x0 (xcor (first-point l)))
                      (x1 (xcor (second-point l)))
                      (sx0 (xcor (car solns)))
                      (sx1 (xcor (cadr solns))))
                  (if (= (sign (- x1 x0))
                         (sign (- sx1 sx0)))
                    solns
                    (list (cadr solns) (car solns))))))))))))

(define (vesica-piscis c1 c2) 
  ;; returns the line through the vesica picis of circles c1 and c2
  (let ((x1 (xcor (center c1)))
        (y1 (ycor (center c1)))
        (x2 (xcor (center c2)))
        (y2 (ycor (center c2)))
        (d1 (radius c1))
        (d2 (radius c2)))
    (if (= y1 y2)
      (let ((l (vesica-piscis (circle (point y1 x1) d1)
                              (circle (point y2 x2) d2))))
        (let ((x3 (xcor (first-point l)))
              (y3 (ycor (first-point l)))
              (x4 (xcor (second-point l)))
              (y4 (ycor (second-point l))))
          (line (point y3 x3) (point y4 x4))))
      (let ((d (* 2 (- y2 y1))))
        (let ((u (/ (- (+ (square d1) (square x2) (square y2))
                       (+ (square d2) (square x1) (square y1)))
                    d))
              (v (/ (* 2 (- x1 x2)) d)))
          (line (point 0 u) (point 1 (+ u v))))))))


(define (intersect-circle-circle c1 c2)
  (if (equal? (center c1) (center c2))
    (callerror '(intersect-circle-circle: same center) (center c1))
    (if (> (- (distance (center c1) (center c2))
              (+ (radius c1) (radius c2)))
           .001)
      (callerror '(intersect-circle-circle: circles too far apart) c1 c2)
      (intersect-circle-line c1 (vesica-piscis c1 c2)))))

(provide intersect)
(define (intersect p q)
  (let ((ints
          (if (and (circle? q) (not (circle? p)))
            (intersect q p)
            (if (circle? p)
              (if (circle? q)
                (intersect-circle-circle p q)
                (if (line? q)
                  (intersect-circle-line p q)
                  (callerror '(intersect: what) p q) ))
              (if (line? p)
                (if (line? q)
                  (intersect-line-line p q)
                  (callerror '(intersect: what) p q) )
                (callerror '(intersect: what) p q) )))))
    (if (= (length ints) 1)
      (car ints)
      (if (= (length ints) 2)
        (if (< (apply distance ints) 0.0001) (apply midpoint ints) ints)
        ints))))

(provide closest)
(define (closest p pts)
  ;  (write (list 'closest p pts))(newline)
  (if (< (distance p (car pts))
         (distance p (cadr pts)))
    (car pts)
    (cadr pts)))

;; Arithmetic with a ruler and compass

(provide sum)
(define (sum a b)
  (let* ((h (horizontal origin))
         (p (right (intersect (circle origin a) h)))
         (q (right (intersect (circle p b) h))))
    (distance origin q)))

(provide difference)
(define (difference a b)
  (let* ((h (horizontal origin))
         (p (right (intersect (circle origin a) h)))
         (q (left (intersect (circle p b) h))))
    (distance origin q)))

(provide product)
(define (product a b)
  (let ((l1 (line origin (point 1 a)))
        (l2 (vertical (point b 0))))
    (ycor (intersect l1 l2))))

(provide divide)
(define (divide y x)
  (ycor (intersect (vertical (point 1 0))
                   (line origin (point x y)))))

(provide reciprocal)
(define (reciprocal x)
  (divide 1 x))

(provide square-root)
(define (square-root a)
  (let* ((c (circle origin (/ (+ a 1) 2)))
         (p (point (/ (- a 1) 2) 0))
         (l (vertical p)))
    (ycor (top (intersect c l)))))

;; Some generic geometric constructions

(provide perpendicular)
; compute the line perpendicular to line l through point p
(define (perpendicular l p)
  (let ((m (slope l))
        (px (xcor p))
        (py (ycor p)))
    (if (vertical? l)
      (horizontal p)
      (if (= m 0)
        (line (point px (ycor (first-point l)))
              (point px (+ (ycor (first-point l)) py)))
        (let ((mm (- (/ 1 m)))
              (b (+ py (* (/ 1 m) px))))
          (funline (lambda (x) (+ b (* mm x))))))))) 

(define (distance-line-point l p)
  (distance p (perpendicular l p)))

(provide bisector)
; line between two points
(define (bisector p q)
  (perpendicular (line p q) (midpoint p q)))

(provide pointfrom)
; parameterized point: t=0 at a, t=1 at b (on line from a to b)
(define (pointfrom a b t)
  (let ((x0 (xcor a))
        (y0 (ycor a))
        (x1 (xcor b))
        (y1 (ycor b)))
    (point (+ x0 (* t (- x1 x0))) (+ y0 (* t (- y1 y0))))))

(provide midpoint)
(define (midpoint p q) (pointfrom p q (/ 1 2)))

;(provide at)
; intersection of (vertical x) and l

(define (point-at l x)
  (point x (+ (offset l) (* (slope l) x))))

; Computing tangents

(provide tangent-circle-point)
; returns the line tangent to circle c closest to point p
; that is perpendicular to the line from p to the center of c
(define (tangent-circle-point c p)
  (let ((cc (center c))
        (r (radius c))
        (px (xcor p))
        (py (ycor p)))
    (let ((l (line cc p))
          (cx (xcor cc))
          (cy (ycor cc)))
      (if (and (= cx px) (= cy py))
        (callerror '(tangent-circle-point: no tangent) c p)
        (let ((pts (intersect c l)))
          (let ((ls (map (lambda (p) (perpendicular l p))
                         pts)))
            (if (null? pts)
              '()
              (if (= (length pts) 1)
                (car ls)
                (if (< (distance p (car pts))
                       (distance p (cadr pts)))
                  (car ls)
                  (cadr ls))))))))))

(provide tangent-circle-line)
(define (tangent-circle-line c l)
  (let ((pts (intersect c l)))
    (map (lambda (p) (tangent-circle-point c p)) pts)))

(provide tangent?)
; is line l tangent to circle c?
(define (tangent? l c)
  (point? (intersect-circle-line c l)))

; tangent-1 has some roundoff error---tangent corrects it by a simple geometric construction...
; (provide better-tangent)

(provide perp-line-circle)
(define (perp-line-circle l c)
  (let* ((pl (perpendicular l (center c)))
         (q (intersect pl l))
         (cl (closest q (intersect pl c))))
    ;    (write (list '?? (- (distance (center c) cl) (radius c))))
    ;    (newline)
    cl))

(provide tangent)    
(define (tangent small big)
  (map (lambda (l)
         (apply line
                (map (lambda (c) (perp-line-circle l c))
                     (list small big))))
       (tangent1 small big)))

(provide tangent1)
(define (tangent1 small big)
  ; finds the two "inner" lines tangent to both c1 and c2, assuming c1, c2 do not intersect...
  (let ((d (line (center small) (center big))))
    (let ((p1 (perpendicular d (center small)))
          (p2 (perpendicular d (center big))))
      (let ((a (left (intersect small p1)))
            (b (right (intersect big p2))))
        (let ((i (intersect d (line a b))))
          (let ((j (midpoint i (center big))))
            (let ((pts (intersect big (circle j (distance i j)))))
              (map (lambda (pt) (line i pt)) pts))))))))

(provide reverse-curve)
; roll a circle of radius outer-radius [see code] along exterior of inner-circle
; and find the positions of its center making it tangent with point pt
(define (reverse-curve inner-circle outer pt)
  (let ((outer-radius (- outer (radius inner-circle))))
    (let ((pts (intersect (circle pt (abs outer-radius))
                          (circle (center inner-circle) 
                                  outer))))
      (map (lambda (p)
             (let ((d (distance p (closest p (intersect inner-circle
                                                        (line p (center inner-circle)))))))

               (circle p d)))
           pts))))

(provide inscribe)
(define (inscribe c1 c2 r)
  ; circles tangent to insides of circles c1, c2 of radius r
  (let ((c1p (circle (center c1) (- (radius c1) r)))
        (c2p (circle (center c2) (- (radius c2) r))))
    (map (lambda (pt) (circle pt r))
         (intersect c1p c2p))))

(provide inscribepoint)
(define (inscribepoint c p r)
  ; circles of radius r tangent to inside of c, and point p
  (let ((c1 (circle (center c) (- (radius c) r)))
        (c2 (circle p r)))
    (map (lambda (pt) (circle pt r))
         (intersect c1 c2))))

(provide outscribepoint)
(define (outscribepoint c p r)
  ; circles of radius r tangent to inside of c, and point p
  (let ((c1 (circle (center c) (+ (radius c) r)))
        (c2 (circle p r)))
    (map (lambda (pt) (circle pt r))
         (intersect c1 c2))))

(provide outscribe)
(define (outscribe c1 c2 r)
  ; circles tangent to outsides of c1, c2 of radius r
  (let ((c1p (circle (center c1) (+ (radius c1) r)))
        (c2p (circle (center c2) (+ (radius c2) r))))
    (map (lambda (pt) (circle pt r))
         (intersect c1p c2p))))

(provide inoutscribe)
(define (inoutscribe c1 c2 r)
  ; circles tangent to inside of c1, outside of c2, of radius r
  (let ((c1p (circle (center c1) (- (radius c1) r)))
        (c2p (circle (center c2) (+ (radius c2) r))))
    (map (lambda (pt) (circle pt r))
         (intersect c1p c2p))))

; for inscribing on the left side of an outline!

(provide inscribeinside-circle-line)
(define (inscribeinside-circle-line bout icirc iline r)
  (let* ((newline (line (xshift (first-point iline) r)
                        (xshift (second-point iline) r)))
         (newcirc (circle (center icirc) (- (radius icirc) r))))
    (circle (bout (intersect newline newcirc)) r)))

(provide inscribeoutside-circle-line)
(define (inscribeoutside-circle-line bout icirc iline r)
  (let* ((newline (line (xshift (first-point iline) r)
                        (xshift (second-point iline) r)))
         (newcirc (circle (center icirc) (- r (radius icirc))))
         (pts (intersect newline newcirc)))
    (if (point? pts)
      (circle pts r)
      (circle (bout pts) r))))

; inscribing squares around circles

(provide outscribesquare)
(define (outscribesquare circ)
  (let* ((r (radius circ))
         (c (center circ))
         (xvec (point r 0))
         (yvec (point 0 r))
         (ur (vec+ (vec+ c xvec) yvec))
         (lr (vec- (vec+ c xvec) yvec))
         (ul (vec+ (vec- c xvec) yvec))
         (ll (vec- (vec- c xvec) yvec)))
    (polygon ur lr ll ul)))

(provide inscribesquare)
(define (inscribesquare circ)
  (outscribesquare (circle (center circ) 
                           (* (radius circ) (/ 1 (sqrt 2))))))

(provide rotated-outscribesquare)
(define (rotated-outscribesquare circ)
  (let* ((r (radius circ))
         (c (center circ))
         (sqrt2 (sqrt 2))
         (xvec (point (* sqrt2 r) 0))
         (yvec (transpose xvec))
         (t (vec+ c yvec))
         (r (vec+ c xvec))
         (b (vec- c yvec))
         (l (vec- c xvec)))
    (polygon t r b l )))

(provide rotated-inscribesquare)
(define (rotated-inscribesquare circ)
  (rotated-outscribesquare (circle (center circ) 
                                   (* (radius circ) (/ 1 (sqrt 2))))))
(provide geometric-section)

(define (geometric-section p q) 
  (pointfrom p q (/ (- 3 (sqrt 5)) 2)))
;(define (geometric-section p q)
;  (let* ((perp-p (perpendicular (line p q) p))
;         (perp-q (perpendicular (line p q) q))
;         (circ (circle p (/ (distance p q) 2)))
;         (r (top (intersect perp-p circ)))
;         (s (intersect (perpendicular perp-p r) perp-q))
;         (t (closest s (intersect (line p s) circ)))
;         (u (closest p (intersect (line r s) (circle s (distance s t)))))
;         (v (intersect (line p q) (perpendicular (line r s) u))))
;    v))


(provide upper-left-flank)
(define (upper-left-flank l c r)
  ; circle of radius r tangent to left side of outline
  ; of vertical line l and inside of circle c
  (let ((l2 (line (xshift (first-point l) r) (xshift (second-point l) r)))
        (c2 (circle (center c) (- (radius c) r))))
    (circle (top (intersect l2 c2)) r)))

(provide upper-right-flank)
(define (upper-right-flank l c r)
  (let ((l2 (line (xshift (first-point l) (- r)) (xshift (second-point l) (- r))))
        (c2 (circle (center c) (- (radius c) r))))
    (circle (top (intersect l2 c2)) r)))

(provide lower-left-flank)
(define (lower-left-flank l c r)
  (let ((l2 (line (xshift (first-point l) r) (xshift (second-point l) r)))
        (c2 (circle (center c) (- (radius c) r))))
    (circle (bottom (intersect l2 c2)) r)))

(provide lower-right-flank)
(define (lower-right-flank l c r)
  (let ((l2 (line (xshift (first-point l) (- r)) (xshift (second-point l) (- r))))
        (c2 (circle (center c) (- (radius c) r))))
    (circle (bottom (intersect l2 c2)) r)))

(provide left-flush)
(define (left-flush c r)
  ; circle of radius r with center of same height as circle c
  (circlefrom (xshift (west c) r) (west c)))

(provide right-flush)
(define (right-flush c r)
  (circlefrom (xshift (east c) (- r)) (east c)))

(provide lower-corner)
(define (lower-corner base-circle moving-circle-radius p)
  (lower-circle (reverse-curve base-circle
                               (+ (radius base-circle) moving-circle-radius)
                               p)))

(provide upper-corner)
(define (upper-corner base-circle moving-circle-radius p)
  (upper-circle (reverse-curve base-circle
                               (+ (radius base-circle) moving-circle-radius)
                               p)))
(provide middle-top-corner)         
(define (middle-top-corner base-circle moving-circle-radius p)
  (lower-circle (reverse-curve base-circle
                               (- (radius base-circle) moving-circle-radius)
                               p)))

(provide middle-bottom-corner)         
(define (middle-bottom-corner base-circle moving-circle-radius p)
  (upper-circle (reverse-curve base-circle
                               (- (radius base-circle) moving-circle-radius)
                               p)))


;;
;; Interface to DRAWING ON THE SCREEN ...
;;

(define xx 1000)

(define yy 1750)

(define (roundup n)
  (+ (* 10 (quotient n 10)) (if (= 0 (remainder n 10)) 0 1)))

(provide hruler)
; horizontal ruler
(define (hruler x y d)
  (let ((d (roundup d)))
    (drawrulersegment (point x y) (point (+ x d) y))
    (let ((n (index (/ d 10))))
      (map (lambda (i) 
             (drawrulersegment (point (+ x (* 10 i)) y) 
                               (point (+ x (* 10 i)) (- y (if (= 0 (remainder i 5)) 12 4))))
             (if (= 0 (remainder i 5))
               (let ((p (map-point  (point (+ x (* 10 i)) (+ y 10)))))
                 (let ((a (xcor p))
                       (b (ycor p)))
                   (let ((m (+ (* 10 (abs (- i (/ d 20)))) 0)))
                     (send dc draw-text (if (< m 300) (~r m) "") a b))))
               'nothing))             
           n))))

(provide vruler)
; vertical ruler
(define (vruler x y d)
  (let ((d (roundup d)))
    (drawrulersegment (point (- x) y) (point (- x) (- y d)))
    (let ((n (index (/ d 10))))
      (map (lambda (i) 
             (drawrulersegment (point (- x) (- y (* 10 i)))
                               (point (+ (- x) (if (= 0 (remainder i 5)) -12 -4)) (- y (* 10 i))))
             (if (= 0 (remainder i 5))
               (let ((p (map-point (point (- 5 x) (- y (* 10 i))))))
                 (let ((a (xcor p))
                       (b (ycor p)))
                   (send dc draw-text (let ((j (* 10 (- i (/ d 20)))))
                                        (if (< (- j) 500) (~r (abs j)) "")) (+ a 5) (- b 8))))
               'nothing))             
           n))))

(define (drawrulersegment p1 p2)
  (let ((p1 (map-point p1))
        (p2 (map-point p2)))
    (send dc set-pen arc-color 0 'solid)
    (send dc draw-line (xcor p1) (ycor p1) (xcor p2) (ycor p2))))

;(define scale 1.5)                         ; for 1 meter width

(define scale (* 1.5 (/ 1000 914.4)))      ; for 36 inch width

; *****************

; a failed attempt at scaling...
(define (mm y pct)
  (- y (* pct (+ y 305) (/ (+ y 305) (+ 431 305)))))

(define (map-point p)
  (let ((x (xcor p))
        (y (ycor p)))
    (let ((x (+ (* scale x) (* 0 y)))

          (y (+ (* 0 x) (* (- scale) y))))
      (point (+ x (/ xx 2)) (+ y (/ yy 2))))))

;(define (mp x y)
;  (make-posn (+ x (quotient xx 2)) (- (quotient yy 2) y)))

;(define (mp x y)
;  (let ((p
;  (point (+ x (quotient xx 2) (xcor origin)) (+ (- (quotient yy 2) y) (ycor origin)))))
;    p))


; *****************


(define (drawit obj)
  (if (circle? obj)
    (drawcircle obj circlecolor)
    (if (line? obj)
      (begin (drawline obj "green")
             (if mirrorp (drawline (mirrorline obj) "green") 'nothing))
      'nothing)))

(define (ddrawit obj) 
  (if drawp (drawit obj) 'drawn))

(define (draw-point p c)
  (if drawp 
    (begin (draw-point-1 p c)
           ;             (if mirrorp (draw-point-1 (mirror p) c) 'nothing))
           (if #f (draw-point-1 (mirror p) c) 'nothing))
    'nothing))

(define (draw-point-1 pt c)
  (let ((p (map-point pt)))
    (send dc set-brush c 'solid)
    (send dc set-pen c 1 'solid)
    (send dc draw-rectangle (- (xcor p) 2) (- (ycor p) 2) 4 4)
    (send dc draw-text (cadr pt)  (xcor p) (ycor p))
    (send dc set-brush c 'transparent)))

(define (edge-line p1 p2)
  (let* ((l (line p1 p2))
         (l1 (perpendicular l p1))
         (l2 (perpendicular l p2))
         (c1 (circle p1 overhang))
         (c2 (circle p2 overhang)))
    (if (= (sign (xcor p1)) (sign (xcor p2)))
      (if (< (xcor p1) 0)
        (draw-solid-line (left (intersect l1 c1)) (left (intersect l2 c2)) "pink")
        (draw-solid-line (right (intersect l1 c1)) (right (intersect l2 c2)) "pink"))
      'nothing)))

(define (drawsolidline p1 p2 c)   
  (let ((p1 (map-point p1))
        (p2 (map-point p2)))
    (send dc set-pen c (if (eq? c arc-color) arc-thickness 0) 'solid)
    (send dc draw-line (xcor p1) (ycor p1) (xcor p2) (ycor p2))))        

(define (draw-solid-line p1 p2 c)
  (drawsolidline p1 p2 c)
  (if (and tracing (eq? c arc-color)) (edge-line p1 p2) 'nothing))

(define (drawline l c)
  (if drawp (begin
              (send dc set-pen c 0 'solid)
              (let ((p1 (map-point (first-point l)))
                    (p2 (map-point (second-point l))))
                (let ((l (line p1 p2)))
                  (if (eq? (slope l) 'infinity)
                    (send dc draw-line (xcor p1) 1 (xcor p1) yy)
                    (let ((f (linefun l)))
                      (send dc draw-line 1 (f 0) xx (f xx)))))))
    'nothing))

(define (drawcircle circ c)
  (if drawp (begin
              (send dc set-pen c 0 'solid)
              (let ((p (map-point (center circ)))
                    (r (* scale (radius circ))))
                (let ((q (vec+ p (scalevec r (point -1 -1)))))
                  (send dc set-brush c 'solid)
                  (send dc set-pen c 0 'solid)
                  (send dc set-brush c 'transparent)
                  (send dc draw-ellipse (xcor q) (ycor q) (* 2 r) (* 2 r)))))
    'nothing))

(define (angle y x)
  ;  (write (list 'ANGLE* y x)) (newline)
  (if (= x 0)
    (if (> y 0)
      (/ pi 2)
      (* 3 (/ pi 2)))
    (if (= y 0)
      (if (> x 0)
        0
        pi)
      (if (> x 0)
        (if (> y 0)
          (atan (/ y x))
          (+ (* pi 1.5) (atan (/ x (- y)))))
        (if (> y 0)
          (+ (* pi 0.5) (atan (/ (- x) y)))
          (+ pi (atan (/ y x))))))))

(define (ang p) (* (/ 360 (* 2 pi)) (angle (ycor p) (xcor p))))

(define (r->ang r) (* r (/ 180 pi)))
(define (ang->r d) (* d (/ pi 180)))

(define (nearline? pt l)
  (< (distance pt (intersect l (perpendicular l pt))) .00000001))

(define (arcrad oo pt)
  (let ((o (map-point oo))
        (p (map-point pt)))
    (if (nearline? p (vertical o))
      (if (< (ycor p) (ycor o)) (* pi 0.5) (* pi 1.5))
      (if (nearline? p (horizontal o))
        (if (> (xcor p) (xcor o)) 0 pi)
        (let ((q (intersect (horizontal o) (vertical p))))
          (begin ;(write '!)
            (angle (- (ycor q) (ycor p)) (- (xcor q) (xcor o)))))))))

;; **********  under construction
;; replacing arc drawing by segments

(define Delta 1)

(define (dsl p1 p2)
  (send dc set-pen arc-color 0 'solid)
  (send dc draw-line (xcor p1) (ycor p1) (xcor p2) (ycor p2)))

(define (darc x y w a b)
  (let ((c (point (+ x (/ w 2)) (- y (/ w 2))))
        (d (* w (- b a))))
    (darciter c (/ w 2) a b (/ Delta d))))

(define (darciter c r a b inc)
  (if (> a b)
    'done
    (begin
      (dsl (map-point (vec+ c (point (* r (cos a)) (* r (sin a)))))
           (map-point (vec+ c (point (* r (cos (+ a inc))) (* r (sin (+ a inc)))))))
      (darciter c r (+ a inc) b inc))))

;(darc 0 0 100 (* pi 0.25) (* pi 1.25))

;; ********** end of construction

(define (extend oo p oc)
  (let* ((d (distance oo p))
         (r (/ (+ d (* oc overhang)) d)))
    (pointfrom oo p r)))

(define (orientation circ)
  (let* ((c (center circ))
         (p (intersect (horizontal c) (vertical origin)))
         (q (closest p (intersect circ (horizontal c)))))
    (if (< (distance c q) (distance c p))
      -1
      +1)))

(define (drawarc oo pt1 pt2 c)
  (let ((oc (orientation (circlefrom oo pt1))))
    (drawarc-a oo pt1 pt2 c)
    (if tracing
      (drawarc-a oo (extend oo pt1 oc) (extend oo pt2 oc) "pink")
      'nothing)))

(define (drawarc-a oo pt1 pt2 c)
  ;  (write (list 'DA (- (distance oo pt1) (distance oo pt2)))) (newline)
  (drawarc-1 oo pt1 pt2 c)
  (if mirrorp (drawarc-1 (mirror oo) (mirror pt1) (mirror pt2) c) 'done))

(define arc-thickness 0)

(provide arcthickness)
(define (arcthickness n)
  (set! arc-thickness n))

(define (drawarc-1 oo pt1 pt2 c)
  ;  (send dc set-pen c 1 'short-dash)
  (drawseg oo pt1)
  (drawseg oo pt2)
  ;  (write (list 'DRAWARC oo pt1 pt2)) (newline)
  (let ((alpha (arcrad oo pt1))
        (beta (arcrad oo pt2))
        (r1 (distance (map-point oo) (map-point pt1)))
        ;code is only using r1, not r2 (for later deformation)
        (r2 (distance (map-point oo) (map-point pt2))))
    (let ((alpha (min alpha beta))
          (beta (max alpha beta))
          (corner (vec+ (map-point oo) (scalevec r1 (point -1 -1)))))
      ;            (write (list (r->ang alpha) (r->ang beta))) (newline)
      (send dc set-pen "yellow" 0 'solid)
      (send dc set-brush "yellow" 'transparent)
      ;            (send dc draw-rectangle (xcor corner) (ycor corner) (* 2 r) (* 2 r))
      (send dc set-pen c arc-thickness 'solid)
      (send dc set-brush c 'transparent)
      (if (> (- beta alpha) pi)
        ; 0.0 was 0.5
        ;              (darc (+ 0.0 (xcor corner)) (+ 0.0 (ycor corner))
        ;                                (* 2 (+ 0.0 r)) 
        ;                                beta (+ alpha (* 2 pi)))
        ;              (darc (+ 0.0 (xcor corner)) (+ 0.0 (ycor corner))
        ;                                (* 2 (+ 0.0 r)) 
        ;                                alpha beta)
        (send dc draw-arc (+ 0.0 (xcor corner)) (+ 0.0 (ycor corner))
              (* 2 (+ 0.0 r1)) (* 2 (+ 0.0 r1)) 
              beta (+ alpha (* 2 pi)))
        (send dc draw-arc (+ 0.0 (xcor corner)) (+ 0.0 (ycor corner))
              (* 2 (+ 0.0 r1)) (* 2 (+ 0.0 r1))
              alpha beta)
        ))))

; arcs by repeated segments


;            (let ((beta (if (> (- beta alpha) pi) (- beta pi) beta)))
;              (send dc draw-arc (xcor corner) (ycor corner) 
;                                (* 2 r) (* 2 r) 
;                                alpha beta)))))


;               (drawarc o pt2 pt1 c)
;               (begin
;                 (write (list p1 q1 p2 q2))(write (list (xdistance o corner)
;                                                        (ydistance o corner)))
;           (write (list (/ (* alpha 360) (* 2 pi))
;                        (/ (* beta 360) (* 2 pi))))
;            (draw-point oo "black") (draw-point pt1 "red") (draw-point pt2 "green")
;            (draw-point corner "red")
;            (send dc set-pen c 1 'solid)
;            (send dc set-brush c 'transparent)
;            (send dc draw-arc (xcor corner) (ycor corner) (* 2 r) (* 2 r) alpha beta)))))))))

(define (polar r theta)
  (point (* r (cos (/ (* theta pi) 180)))
         (* r (sin (/ (* theta pi) 180)))))


(define (draw-solid-disk p n c) 'disk)
;  (send dc draw-ellipse (xcor p) (ycor p) (+ (xcor p) 3) (+ (ycor p) 3)))

(define (draw-solid-string p s) 'string)

; (draw-solid-disk (make-pixel (xcor p) (ycor p)) 3 'blue)
;    (draw-solid-string (make-pixel (+ (xcor p) 10) (+ (ycor p) 10)) s)))

; *****************


;(drawarc origin (polar 200 -45) (polar 200 45) "green")

;(define corner (map-point (point -200 200)))
;(draw-point (point -200 200) "red")
;(send dc set-pen "green" 1 'solid)
;(send dc set-brush "green" 'transparent)
;(send dc draw-arc (xcor corner) (ycor corner) 400 400 (* pi 1.75) (* pi 2.25))

;  NOW I PATCH IN...

(define (drawsegment p1 p2)
  (draw-solid-line p1 p2 arc-color)
  (if mirrorp (draw-solid-line (mirror p1) (mirror p2) arc-color) 'done))

(define (drawseg p1 p2)
  (if drawp
    (draw-solid-line p1
                     p2
                     "orange")
    'foo))


(define (drawtangent p1 p2)
  (begin (drawsegment p1 p2)
         (if mirrorp (drawsegment (mirror p1) (mirror p2)) 'done)))

(define (label-1 s p)
  (begin
    (draw-solid-disk (map-point(xcor p) (ycor p)) 3 arc-color)
    (draw-solid-string (map-point(+ (xcor p) 10) (+ (ycor p) 10)) s)))

(provide label)
(define (label s p)
  (list 'point s (xcor p) (ycor p)))
;  (if drawp
;      (begin
;        (label-1 s p)
;        (if (< (xcor p) 0) (label-1 (string-append s "'") (mirror p)) 'foo)
;        p)
;      p))


;; Drawing stuff

;; Drawing curves with starting point, ending point, and a series of circle arc

(define (next p intersections)
  (if (null? intersections)
    (callerror '(next: what) p intersections)
    (if (point? intersections)
      intersections
      (if (< (distance p (car intersections))
             (distance p (cadr intersections)))
        (car intersections)
        (cadr intersections)))))

(define (last? l)
  (null? (cdr l)))

(provide make-curve)
(define (make-curve start finish stuff)
  (list 'curve start finish stuff))

; moving a curve in the y-direction: for bricolage...

(provide ymove)
(define (ymove s d)
  (if (null? s)
    '()
    (if (point? s)
      (label (labelof s) (point (xcor s) (+ (ycor s) d)))
      (if (line? s)
        (line (ymove (first-point s) d) (ymove (second-point s) d))
        (if (circle? s)
          (circle (ymove (center s) d) (radius s))
          (if (begins-with? s 'curve)
            (make-curve (ymove (cadr s) d) (ymove (caddr s) d) (map (lambda (a) (ymove a d)) (cadddr s)))
            (if (begins-with? s 'arc)
              (apply makearc (map (lambda (a) (ymove a d)) (cdr s)))
              (if (begins-with? s 'tangent)
                (callerror '(ymove: tangent?) s)
                (if (begins-with? s 'segment)
                  (apply makeseg (map (lambda (a) (ymove a d)) (cdr s)))
                  (map (lambda (a) (ymove a d)) s))))))))))


(define (draw-curve start finish objects)
  (curve-1 (if (line? (car objects))
             start
             (closest start (intersect (car objects)
                                       (line (center (car objects)) start))))
           finish
           objects))

(define (curve-1 start finish objects)
  (if (last? objects)
    (if (line? (car objects))
      (drawsegment start finish)
      (let ((cc (center (car objects)))
            (rr (distance (center (car objects)) start)))
        (let ((newfinish (closest finish (intersect (line cc finish)
                                                    (circle cc rr)))))
          (drawarc cc start newfinish arc-color))))
    (let ((p (next start
                   (intersect (car objects) (cadr objects)))))
      (begin
        (if (line? (car objects))
          (drawsegment start p)
          (drawarc (center (car objects)) start p arc-color))
        (curve-1 p finish (cdr objects))))))

(provide makearc)
(define (makearc x y z) (list 'arc x y z))

(provide maketangent)
(define (maketangent x y) (list 'tangent x y))

(provide makeseg)
(define (makeseg x y) (list 'segment x y))

(provide sketch)
(define (sketch instrument)
  (if (null? instrument)
    '()
    (if (point? instrument)
      (draw-point instrument "red")
      (if (or (line? instrument) (circle? instrument))
        (drawit instrument)
        (if (begins-with? instrument 'curve)
          (apply draw-curve (cdr instrument))
          (if (begins-with? instrument 'arc)
            (drawarc (cadr instrument) (caddr instrument) (cadddr instrument) arc-color)
            (if (begins-with? instrument 'tangent)
              (apply drawtangent (cdr instrument))
              (if (begins-with? instrument 'segment)
                (apply drawseg (cdr instrument))
                (map sketch instrument)))))))))

; a thicker-drawn circle
(provide enhance)
(define (enhance obj)
  (if (circle? obj)
    (enhance-circle obj)
    (if (line? obj)
      (enhance-line obj)
      (callerror '(callerror what?) obj))))

(define (enhance-line L)
  (let* ((x1 (xcor (first-point L)))
         (y1 (ycor (first-point L)))
         (x2 (xcor (second-point L)))
         (y2 (ycor (second-point L)))
         (el (list L
                   (line (point (d+ x1) (d+ y1)) (point (d+ x2) (d+ y2)))
                   (line (point (d- x1) (d- y1)) (point (d- x2) (d- y2))))))
    (if (vertical? L)
      (cons (line (point (+ 1  x1) (+ 1 y1)) (point (+ 1 x2) (+ 1 y2))) el)
      (if (horizontal? L)
        (cons (line (point (+ 1  x1) (+ 1 y1)) (point (+ 1 x2) (+ 1 y2))) el)
        el))))

(define (enhance-circle C)
  (let ((c (center C))
        (r (radius C)))
    (list C
          (circle c (d+ r))
          (circle c (d- r)))))

(provide polygon)
(define polygon
  (lambda s (segments (apply list (append s (list (car s)))))))

(provide segments)
(define segments 
  (lambda (l)
    (segs (car l) (cdr l))))

(define (segs p l)
  (cons (makeseg p (car l))
        (if (last? l)
          '()
          (segs (car l) (cdr l)))))

(provide dc)
; initialize graphic output
; choose a dest through a dialog

;(define dc (new pdf-dc% [interactive #f] [width 1200] [height 1600])) 

(define stdout (current-output-port))
(current-output-port (open-output-nowhere))
(current-error-port (open-output-nowhere))

(define dc (new svg-dc% [width 1000] [height 1300] [output stdout])) 

(send dc start-doc "...")
(send dc start-page)
(send dc set-smoothing 'smoothed)

(send dc set-pen "red" 0 'solid)

(provide end-drawing)
(define (end-drawing)
  ; ;;;;
  ;(send dc erase)
  (hruler -400 475 800) 
  (vruler -250 700 1400)


  (send dc set-font (make-font #:size 15 #:family 'default
                               #:weight 'bold))
  (send dc set-text-foreground "black")
  ;(send dc draw-rectangle 0 0 500 30)
  (send dc draw-text drawing-title 100 50)

  (send dc set-font (make-font #:size 5 #:family 'default
                               #:weight 'bold))
  (send dc draw-text coder 25 25)
  ; ;;;;
  (send dc end-page)
  (send dc end-doc))



(module+ test
         ;; Tests to be run with raco test
         )

(module+ main
         ;; Main entry point, executed when run with the `racket` executable or DrRacket.
         )

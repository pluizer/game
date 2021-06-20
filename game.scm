(import coati
	    ssax
	    (chicken string)
	    (chicken pathname)
        (chicken time)
	    matchable
	    srfi-1
        srfi-4)


(define (%create-mapping-lambda #!key width height tiles-w tiles-h layers)
  (lambda (l coord)
    (let ((x (coord:x coord))
	      (y (coord:y coord)))
      (if (and (>= x 0)
	           (< x width)
	           (>= y 0)
	           (< y height))
	      (let ((sprite (vector-ref (vector-ref (vector-ref layers l) x) y)))
	        sprite)
	      #f))))


(define sprite-map
  '(chair
    two
    two-base
    two-top
    (four 1 1)
    (four-base 1 1)
    (four-top 1 1)
    (block 1 1)
    door-closed
    door-open
    (floor 1 1)
    (wall-lamp 1 2)
    three
    (table 1 2)
    table-lamp
    three-base
    three-top
    wall
    (wall 1 2)
    (wall-top 1 2)))

;; Two maps, (a and b) projected over the infinite map ...
;; +----------+----------+----------+----------+----------+
;; |(0,1)     |(0,2)     |(0,3)     |(0,4)     |(0,5)     |
;; |          |          |          |          |          |
;; |    b(0,0)|    b(0,1)|          |          |          |
;; +----------+----------+----------+----------+----------+
;; |(1,1)     |(1,2)     |(1,3)     |(1,4)     |(1,5)     |
;; |          |          |          |          |          |
;; |    b(1,0)|    b(1,1)|          |          |          |
;; +----------+----------+----------+----------+----------+
;; |(2,1)     |(2,2)     |(2,3)     |(2,4)     |(2,5)     |
;; |          |          |          |          |          |
;; |          |    a(0,0)|    a(0,1)|    a(0,3)|          |
;; +----------+----------+----------+----------+----------+
;; |(3,1)     |(3,2)     |(3,3)     |(3,4)     |(3,5)     |
;; |          |          |          |          |          |
;; |          |    a(1,0)|    a(1,1)|    a(1,3)|          |
;; +----------+----------+----------+----------+----------+
;; |(4,1)     |(4,2)     |(4,3)     |(4,4)     |(4,5)     |
;; |          |          |          |          |          |
;; |          |    a(2,0)|    a(2,1)|    a(2,3)|          |
;; +----------+----------+----------+----------+----------+
;; |(5,1)     |(5,2)     |(5,3)     |(5,4)     |(5,5)     |
;; |          |          |          |          |          |
;; |          |          |          |          |          |
;; +----------+----------+----------+----------+----------+

(define clear-color (rgb:create 1 1 1))

(define (make-button)
  (let ((texture (texture:create (vect:create 100 100))))
    (with-target texture (texture:clear! (rgb:create 0 1 0)))
    (texture:renderer texture)))

(define (tile-func sprite sprite-map mapping-lambda)
  (lambda (coord)
	(let ((sprite (mapping-lambda 1 coord)))
	  (if sprite sprite
		  (sprite-map:lookup sprite-map 'chair 'north)))))

(define (game)
  (let* (
	     (texture (texture:load "share/tiles.png"))
	     (sprite-map (sprite-map:create texture 4 15 sprite-map))
         (sprite (sprite-map:lookup sprite-map 'four 'north))
	     (batcher (sprite-batcher:create))
         (root-node      (node:create-root))
         (wall-node      (spawn-node! (sprite-node batcher sprite) root-node
                                      (trans:create (vect:create 0 0)
                                                    origin: (vect:create 0 0)
                                                    flip-v?: #t)))


	     ;; (root (node:create-root))
	     ;; (node (spawn-node! (sprite-node batcher (sprite-map:lookup sprite-map 'chair 'north))
	     ;; 		    root
	     ;; 		    (trans:create (vect:create 0.0 0.0)
	     ;; 				  origin:
	     ;; 				  (vect:create .5 .5))))
	     (tilemap (tilemap:create isometric?: #t))
	     (tilemap-2 (tilemap:create isometric?: #t))
	     (tilemap-3 (tilemap:create isometric?: #t))
	     (the-map (open-tile-map "share/map2.tmx"))
	     (mapping-lambda (apply %create-mapping-lambda the-map))
         (button (make-button))
	     )
    (listen-for-event `(mouse-move)
                      (lambda (pos)
                        (display pos)
                        (newline)))
    (lambda (#!key (pos 0)
                   (frame-count 0)
                   (epoch (current-milliseconds)))
      (let* ((milli (current-milliseconds))
             (passed? (> (- milli epoch) 1000))
             (camera (camera:create (vect:create pos 0) (vect:create 20 10)))

             )
        ;; (camera-pos-set! camera (vect:create 0 pos))
        (texture:clear! (rgb:create 1 1 1))
        (with-blending trans(rgb:create 1 1 1)
		               (with-texture texture
				                     (with-camera/proc camera
						                               (lambda ()
                                                         (tilemap:render tilemap
                                                                         #f
								                                         tile-func sprite sprite-map mapping-lambda)
                                                         (sprite-batcher:render batcher)))))

        ;; (button)
        (when passed? (display frame-count) (newline))
        (if (key-down? key-escape)
	        #f
	        (list pos: (- pos 0.01)
                  frame-count: (if passed? 0 (+ frame-count 1))
                  epoch: (if passed? milli epoch)))))))

(coati:init 1000 500 "" #t)
(coati:start game)
(coati:close)
(exit)

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)
(define TANK-Y (- HEIGHT 30))

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define LAYER (rectangle WIDTH HEIGHT "solid" "transparent"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

;;========================================
;;             🚔 TANK 🚔
;;========================================
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;;========================================
;;             👾 INVADER 👾
;;========================================
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1.5))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1.5))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1.5)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of Invader

(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI3 (list I1 I2 I3))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;;========================================
;;             🚀 MISSILE 🚀
;;========================================

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of Missile

(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM3 (list M1 M2 M3))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Function definitions:

;; Game -> Game
;; start the world with (main G0)

(define (main g)
  (big-bang g             ; Game
    (on-tick   tock)      ; Game -> Game
    (to-draw   render)    ; Game -> Image ✅
    (stop-when end?)      ; Game -> Boolean ✅
    (on-key    control))) ; Game KeyEvent -> Game ✅

;; Game -> Game
;; produce the next state of game

(define (tock g)
  (collision (make-game (next-invaders (spawn (game-invaders g)))
                        (advance-missiles (game-missiles g) (game-invaders g))
                        (advance-tank (game-tank g)))))

;; TODO:
;;  - Random invader (spawn )
;;  - Handle collision

;; Game -> Game
;; remove all the collision of missiles and invaders

(define (collision g)
  (make-game (invaders-col (game-invaders g) (game-missiles g))
             (missiles-col (game-missiles g) (game-invaders g))
             (game-tank g)))

;; ListOfMissile ListOfInvader -> ListOfMissile
;; produce new missile, remove collision

(define (missiles-col lom loi)
  (cond [(empty? lom) empty]
        [else
         (if  (collide-vs-invaders (first lom) loi)
              (missiles-col (rest lom) loi)
              (cons (first lom) (missiles-col (rest lom) loi)))]))

;; Missile ListOfInvader -> ListOfMissile
(define (collide-vs-invaders m loi)
  (cond [(empty? loi) false]
        [else
         (if (collide-vs-missile? (first loi) m)
             true
             (collide-vs-invaders m (rest loi)))]))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; produce new invaders, remove collision

(define (invaders-col loi lom)
  (cond [(empty? loi) empty]
        [else
         (if  (collide-vs-missiles? (first loi) lom)
              (invaders-col (rest loi) lom)
              (cons (first loi) (invaders-col (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; produce true if one of missile hit invader

(define (collide-vs-missiles? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collide-vs-missile? i (first lom))
             true
             (collide-vs-missiles? i (rest lom)))]))
;; Invader Missile -> Boolean
;; produce true if delta x and delta y is smaller than HIT-RANGE
(define (collide-vs-missile? i m)
  (and (< (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (< (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;======================================================================================

;; ListOfInvader -> ListOfInvader
;; randomly spawn new invader

(define (spawn loi)
  (if (= (random 30) 15)
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
      loi))

;; ListOfInvader -> ListOfInvader
;; produce the next position of the Invader, change dx if needed

(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (next-invaders (rest loi)))]))

(define (next-invader i)
  (make-invader (+ (invader-x i) (invader-dx i))
                (+ (invader-y i) INVADER-Y-SPEED)
                (invader-vel i)))

;; Invader -> Integer
;; produce invader-dx, change (+) (-) if hit the wall
(define (invader-vel i)
  (cond [(and (<= (invader-x i) 0) (< (invader-dx i) 0)) (abs (invader-dx i))]
        [(and (>= (invader-x i) WIDTH) (> (invader-dx i) 0)) (* -1 (invader-dx i))]
        [else (invader-dx i)]))

;======================================================================================

;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; advance next list of Missiles, resolve for collision with invaders and remove those out of screen

(define (advance-missiles lom loi)
  (next-missiles (outbound-missiles lom)))

;; ListOfMissiles -> ListOfMissiles
;; remove missiles that (missile-y < 0)

(define (outbound-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (inRangeMissile? (first lom))
             (cons (first lom) (outbound-missiles (rest lom)))
             (outbound-missiles (rest lom)))]))

(define (inRangeMissile? m)
  (> (missile-y m) 0))

;; ListOfMissile -> ListOfMissile
;; minus MISSILE-SPEED to all (missile-y)
(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missile (first lom))
               (next-missiles (rest lom)))]))

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;======================================================================================
 
;; Tank -> Tank
;; produce the next tank

(define (advance-tank t)
  (if (tank-bound? (+ (* TANK-SPEED (tank-dir t)) (tank-x t)))
      (make-tank (+ (* TANK-SPEED (tank-dir t)) (tank-x t)) (tank-dir t))
      t))

(define (tank-bound? x)
  (<= 0 x WIDTH))


;; =========================================
;;                 RENDERING ✅
;; =========================================
;; Game -> Image
;; render the Game state on to BACKGROUND

(define (render g)
  (overlay (render-invaders (game-invaders g))
           (render-missiles (game-missiles g))
           (render-tank (game-tank g))))

;; ListOfInvaders -> Image
;; produce all the invader with correct position on the LAYER

(define (render-invaders loi)
  (cond [(empty? loi) LAYER]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi)))]))

;; ListOfMissiles -> Image
;; produce all the missile with correct position on the LAYER

(define (render-missiles lom)
  (cond [(empty? lom) LAYER]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom)))]))
;; Tank -> Image
;; produce the tank with correct position on the BACKGROUND
(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND))

;; =========================================
;;                  END? ✅
;; =========================================
;; Game -> Boolean
;; produce true if any of the Invader reached the base

(define (end? s)
  (check-invaders (game-invaders s)))

;; ListOfInvader -> Boolean
;; produce true if any of the Invader reached the base

(define (check-invaders loi)
  (cond [(empty? loi) false]
        [else
         (if (reached-base? (first loi))
             true
             (check-invaders (rest loi)))]))

;; Invader -> Boolean
;; produce true if (invader-y) >= HEIGHT
(define (reached-base? i)
  (>= (invader-y i) HEIGHT))

;; =========================================
;;                 CONTROL ✅
;; =========================================
;; Game KeyEvent -> Game
;; change tank direction if KeyEvent is "left" or "right", shoot when KeyEvent is " "

(define (control g ke)
  (cond [(key=? ke " ") (shoot g)]
        [(key=? ke "left") (turn-left g)]
        [(key=? ke "right") (turn-right g)]
        [else g]))

(define (shoot g)
  (make-game (game-invaders g)
             (cons (make-missile (tank-x (game-tank g)) TANK-Y) (game-missiles g))
             (game-tank g)))

(define (turn-left g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) -1)))

(define (turn-right g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) 1)))

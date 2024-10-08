;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise43) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS:
; A WorldState is a Number.
; It represents the distance of the RIGHT MOST EDGE of the car from the left edge of the canvas.

; CONSTANT DEFINITIONS:
(define WHEEL-RADIUS 5)
(define SPEED 3)
(define BG-HEIGHT 200)
(define BG-WIDTH 800)
(define CAR-Y-POS
  (- BG-HEIGHT
     (/ (+ (* WHEEL-RADIUS 2)
           (* WHEEL-RADIUS 3)
           (* WHEEL-RADIUS 2))
        2)))
(define CAR-WIDTH (* WHEEL-RADIUS 12))
  

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))

(define TWO-WHEEL
  (beside
   WHEEL
   (beside
    (rectangle (* WHEEL-RADIUS 5) 0 "solid" "white")
    WHEEL)))

(define CAR
  (above
   (rectangle (* WHEEL-RADIUS 8) (* WHEEL-RADIUS 2) "solid" "red")
   (above
    (rectangle CAR-WIDTH (* WHEEL-RADIUS 3) "solid" "red")
   TWO-WHEEL)))

(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define BACKGROUND
  (place-image TREE
               (/ BG-WIDTH 2)
               (- BG-HEIGHT (/ (image-height TREE) 2))
               (empty-scene BG-WIDTH BG-HEIGHT)))

; FUNCTION DEFENITIONS:
; WorldState -> WorldState
; add SPEED to the current WorldState
(check-expect (tock 0) 3)
(check-expect (tock 999) 1002)
(define (tock cw)
  (+ cw SPEED))

; WorldState -> Image
; produce a image of the current world given the WorldState
(check-expect (render -50) BACKGROUND)
(check-expect (render 0) (place-image CAR (- 0 (/ CAR-WIDTH 2)) CAR-Y-POS BACKGROUND))
(check-expect (render 100) (place-image CAR (- 100 (/ CAR-WIDTH 2)) CAR-Y-POS BACKGROUND))
(check-expect (render (+ BG-WIDTH 999999)) BACKGROUND)

(define (render cw)
  (cond
    [(and (> cw 0)
          (< cw (+ BG-WIDTH CAR-WIDTH)))
     (place-image CAR (- cw (/ CAR-WIDTH 2)) CAR-Y-POS BACKGROUND)]
    [else BACKGROUND]))

; WorldState -> Boolean
; produce true if the WorldState is bigger than (+ BG-WIDTH (/ CAR-WIDTH 2))
(check-expect (end? -5267) false)
(check-expect (end? (/ BG-WIDTH 2)) false)
(check-expect (end? (+ BG-WIDTH 999999)) true)
(define (end? cw)
  (> cw (+ BG-WIDTH CAR-WIDTH)))

; WorldSate -> WorldState
; main function that launch the program.
(define (main initial-state)
  (big-bang initial-state
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))
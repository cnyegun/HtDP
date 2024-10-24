;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise44) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;DATA DEFINITIONS:

; An AnimationState is a Natural.
; interpretation the number of clock ticks
; since the animation started.

;CONSTANT DEFINITIONS:

(define BG-WIDTH 800)
(define BG-HEIGHT 200)
(define WHEEL-WIDTH 10)
(define SPEED 3) ; pixels per clock tick

(define BACKGROUND (empty-scene BG-WIDTH BG-HEIGHT))
(define WHEEL
  (rectangle WHEEL-WIDTH (/ WHEEL-WIDTH 2) "solid" "black"))
(define TWO-WHEELS
  (beside
   WHEEL
   (beside
    (rectangle (* 2 WHEEL-WIDTH) 0 "solid" "white")
    WHEEL)))
(define CAR-BODY
  (rectangle (* WHEEL-WIDTH 5) (* WHEEL-WIDTH 2) "solid" "red"))
(define CAR
  (above
   TWO-WHEELS
   (above
    CAR-BODY
    TWO-WHEELS)))
  
;FUNCTION DEFINITIONS:

; WorldState -> WorldState
; add 1 clock tick to the given WorldState
(check-expect (tock 3) 4)
(check-expect (tock 0) 1)
(define (tock cw) (+ cw 1))

; WorldState -> Image
; render the car's image based on how many clock ticks passed
(check-expect (render 3) (place-image CAR (+ (/ (image-width CAR) 2) (* 3 SPEED)) (* 100 (sin 3)) BACKGROUND))
(check-expect (render (+ BG-WIDTH 999)) BACKGROUND)
(define (render cw)
  (place-image CAR (+ (/ (image-width CAR) 2) (* cw SPEED)) (* 100 (sin cw)) BACKGROUND))

; WorldState -> Boolean
; return true if the car passed the window's margin
(check-expect (end? 3) false)
(check-expect (end? 9999) true)
(define (end? cw)
  (> (* cw SPEED) BG-WIDTH))

; WorldState -> WorldState
; main function that launchs the program
(define (main initial-state)
  (big-bang initial-state
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))
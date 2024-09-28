;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; Display happiness gauge
; gauge-prog

; DATA DEFINITION
; A HappinessGauge is a Natural
; interp. the happiness level

; CONSTANT DEFINITION
(define BG-WIDTH 700)
(define BG-HEIGHT 50)

(define BACKGROUND (empty-scene BG-WIDTH BG-HEIGHT))

; FUNCTION DEFINITION
; WorldState -> WorldState
; decrease -0.01 HappinessGauge when a clock ticks
(check-expect (tock 100) (- 100 0.01))
(check-expect (tock 25) (- 25 0.01))
(check-expect (tock 0.01) 0)
(define (tock cw)
  (- cw 0.01))

; WorldState -> Image
; display the image of 
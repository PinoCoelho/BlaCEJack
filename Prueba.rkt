#lang racket/gui

;; Definimos la primera pantalla
(require 2htdp/image
         (only-in mrlib/image-core render-image))
(provide (all-defined-out))

(define image1 (make-object bitmap% "Blackjack.jpg"))
(define my-frame  (new frame% [label "Black CE Jack"]
       [width 300]
       [height 400]
       [style '(no-resize-border)]))
(define mcan%
  (class canvas%
    (override  on-paint)
    (define on-paint
      (lambda()(send (send this get-dc) 
      draw-bitmap image1 0 0)))        
    (super-instantiate())))

(send my-frame show #t)

(define mcan (new mcan% (parent my-frame)
                  (min-width (image-width image1))
                  (min-height (image-height image1))))

(define drawer (send mcan get-dc))

(define panel (new vertical-panel%
                  [parent frame]))

(define button (new button%
                    [parent panel]
                    [label "Empezar"]
                    [callback (lambda (button event)
                                (send frame show #f) ; Oculta esta pantalla
                                (nueva-pantalla))])) ; Abre la siguiente pantalla

;; Definimos la segunda pantalla (inicialmente oculta)
(define new-frame #f)

(define (nueva-pantalla)
  (set! new-frame (new frame%
                        [label "Juego de Blackjack"]
                        [width 700]
                        [height 700]))

  ;; División de la pantalla del crupier
  (define crupier-panel (new vertical-panel%
                          [parent new-frame]
                          [stretchable-width #t]
                          [stretchable-height #t]
                          [alignment '(center center)]))

  (define crupier-canvas (new canvas%
                            [parent crupier-panel]
                            [paint-callback
                             (lambda (canvas dc)
                               (send dc set-font (make-object font% 14 'default 'normal 'normal))
                               (send dc draw-text "Crupier" 50 50)
                               )]))

  ;; División del panel de jugadores
  (define jugadores-panel (new horizontal-panel%
                            [parent new-frame]
                            [stretchable-width #t]
                            [stretchable-height #t]
                            [alignment '(center center)]))

  ;; División del panel de jugadores en tres partes iguales
  (define jugador1-panel (new vertical-panel%
                            [parent jugadores-panel]
                            [stretchable-width #t]
                            [stretchable-height #t]))

  (define jugador2-panel (new vertical-panel%
                            [parent jugadores-panel]
                            [stretchable-width #t]
                            [stretchable-height #t]))

  (define jugador3-panel (new vertical-panel%
                            [parent jugadores-panel]
                            [stretchable-width #t]
                            [stretchable-height #t]))

  ;; Botones para los jugadores

  (define jugador1-canvas (new canvas%
                              [parent jugador1-panel]
                              [paint-callback
                               (lambda (canvas dc)
                                 (send dc set-font (make-object font% 14 'default 'normal 'normal))
                                 (send dc draw-text "Jugador 1" 50 50)
                                 )]))

  (define jugador1-pedir (new button%
                            [parent jugador1-panel]
                            [label "Pedir"]))

  (define jugador1-plantarse (new button%
                                [parent jugador1-panel]
                                [label "Plantarse"]))

  (define jugador2-canvas (new canvas%
                              [parent jugador2-panel]
                              [paint-callback
                               (lambda (canvas dc)
                                 (send dc set-font (make-object font% 14 'default 'normal 'normal))
                                 (send dc draw-text "Jugador 2" 50 50)
                                 )]))

  (define jugador2-pedir (new button%
                            [parent jugador2-panel]
                            [label "Pedir"]))

  (define jugador2-plantarse (new button%
                                [parent jugador2-panel]
                                [label "Plantarse"]))

  (define jugador3-canvas (new canvas%
                              [parent jugador3-panel]
                              [paint-callback
                               (lambda (canvas dc)
                                 (send dc set-font (make-object font% 14 'default 'normal 'normal))
                                 (send dc draw-text "Jugador 3" 50 50)
                                 )]))

  (define jugador3-pedir (new button%
                            [parent jugador3-panel]
                            [label "Pedir"]))

  (define jugador3-plantarse (new button%
                                [parent jugador3-panel]
                                [label "Plantarse"]))

  (send new-frame show #t))

(send frame show #t)

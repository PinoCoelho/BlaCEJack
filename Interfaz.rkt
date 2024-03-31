#lang racket/gui

(require 2htdp/image
         (only-in mrlib/image-core render-image))

(define image1 (make-object bitmap% "fondo2.jpg"))

(define my-frame  (new frame% [label "Black CE Jack"]
                    [width 300]
                    [height 450]
                    [style '(no-resize-border)]))

(define panel (new vertical-panel%
                  [parent my-frame]))

(define mcan%
  (class canvas%
    (override on-paint)
    (define (on-paint)
      (define dc (send this get-dc))
      (send dc draw-bitmap image1 0 0))
    (super-instantiate ())))

(send my-frame show #t)

(define mcan (new mcan% (parent my-frame)
                       (min-width (send image1 get-width))
                       (min-height (send image1 get-height))))

(define button (new button%
                    [parent panel]
                    [label "Empezar"]
                    [callback (lambda (button event)
                                (send my-frame show #f)
                                (nueva-pantalla))]))

(define new-frame #f)

(define crupier-image (make-object bitmap% "crupier1.png")) ; Cambio aquí a la imagen del crupier

(define (nueva-pantalla)
  (set! new-frame (new frame%
                        [label "Juego de Blackjack"]
                        [width 700]
                        [height 700]))

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
                               (send dc draw-bitmap crupier-image 200 70))])) ; Mostrar la imagen del crupier aquí
                            
  (define jugadores-panel (new horizontal-panel%
                            [parent new-frame]
                            [stretchable-width #t]
                            [stretchable-height #t]
                            [alignment '(center center)]))

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

  (define jugador1-canvas (new canvas%
                              [parent jugador1-panel]
                              [paint-callback
                               (lambda (canvas dc)
                                 (send dc set-font (make-object font% 14 'default 'normal 'normal))
                                 (send dc draw-text "Jugador 1" 50 10)
                                 )]))

  (define jugador1-pedir (new button%
                            [parent jugador1-panel]
                            [label "Pedir"]
                            [callback (lambda (button event)
                                        (cambiar-imagen-crupier))])) ; Cambiar la imagen del crupier al presionar "Pedir" del jugador 1

  (define jugador1-plantarse (new button%
                                [parent jugador1-panel]
                                [label "Plantarse"]))

(define jugador2-canvas (new canvas%
                              [parent jugador2-panel]
                              [paint-callback
                               (lambda (canvas dc)
                                 (send dc set-font (make-object font% 14 'default 'normal 'normal))
                                 (send dc draw-text "Jugador 2" 50 10)
                                 )]))

  (define jugador2-pedir (new button%
                            [parent jugador2-panel]
                            [label "Pedir"]
                            [callback (lambda (button event)
                                        (cambiar-imagen-crupier))]))

  (define jugador2-plantarse (new button%
                                [parent jugador2-panel]
                                [label "Plantarse"]))

  (define jugador3-canvas (new canvas%
                              [parent jugador3-panel]
                              [paint-callback
                               (lambda (canvas dc)
                                 (send dc set-font (make-object font% 14 'default 'normal 'normal))
                                 (send dc draw-text "Jugador 3" 50 10)
                                 )]))

  (define jugador3-pedir (new button%
                            [parent jugador3-panel]
                            [label "Pedir"]
                            [callback (lambda (button event)
                                        (cambiar-imagen-crupier))])) ; Cambiar la imagen del crupier al presionar "Pedir"

  (define jugador3-plantarse (new button%
                                [parent jugador3-panel]
                                [label "Plantarse"]))

  (send new-frame show #t))

(define (cambiar-imagen-crupier)
  ;; Obtener la lista de archivos de imágenes en la carpeta "cards"
  (define crupier-images-directory "cards/")
  (define crupier-images (directory-list crupier-images-directory))

  ;; Seleccionar una imagen aleatoria de la lista
  (define random-image (list-ref crupier-images (random (length crupier-images))))

  ;; Construir la ruta completa de la imagen
  (define image-path (build-path crupier-images-directory random-image))

  ;; Actualizar la imagen del crupier
  (set! crupier-image (make-object bitmap% image-path))
  (send new-frame refresh)) ; Refrescar el frame para mostrar la nueva imagen


(send my-frame show #t)

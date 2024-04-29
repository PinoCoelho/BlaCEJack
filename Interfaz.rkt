#lang racket/gui

; Bibliotecas utilizadas
(require "Logica.rkt")
(require 2htdp/image
         (only-in mrlib/image-core render-image))
(provide (all-defined-out))

; Variables del estado del juego
(define board-list '()) ; ((cartas-disponibles) ((jugador1 (cartas) estado) ... ))
(define player-list '())
(define positions '((350 100) (130 290) (350 320) (580 290)))
(define score-table '())
(define font-size 30)

; Interfaz Gráfica
(define image1 (make-object bitmap% "fondo2.jpg"))
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

(define mcan (new mcan% (parent my-frame)
                  (min-width (image-width image1))
                  (min-height (image-height image1))))

(define drawer (send mcan get-dc))

(define my-frame2  (new frame% [label "Black CE Jack Score Table"]
       [width 400]
       [height 400]
       [style '(no-resize-border)]))

(define mcan2 (new mcan% (parent my-frame2)
                  (min-width 700)
                  (min-height 400)) )

(define row1
  (new horizontal-panel%
       [parent my-frame]
       [min-width   30]
       [min-height   50]
       [style       '(border)]
       [stretchable-height #f]
       [horiz-margin 220]))

(define ask-card (new button%
                    [parent row1]
                    [label "Pedir carta"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (cond
                                  ((= 0 (length (get-deck board-list))) (writeln "no hay cartas"))
                                  (else   
                                   (set! board-list (hand-out board-list 1))
                                   (draw-everyone-cards (get-players board-list) #f)
                                   (draw-image "Deck/52.png" 700 11 0.38001)
                                   (draw-text (number->string (length (get-deck board-list))) 40 "black" 720 40)
                                   )
                                )
                           )]
                    ))

(define stay (new button%
                    [parent row1]
                    [label "Plantarse"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (set! board-list (set-stay board-list))
                                (cond
                                  ((everyone-done? (get-players board-list))
                                   (set! board-list (crupier-IA (set-crupier-first board-list)))
                                   
                                   (draw-everyone-cards (get-players board-list) #t)
                                   (set! score-table (sort-scores(get-scores (get-players board-list))))
                                   
                                   (set! drawer (send mcan2 get-dc))
                                   ;(send drawer set-font (make-object font% font-size 'default))
                                   (show-scores)
                                    
                                  )
                                  (else
                                   (draw-label (current-player-name (get-players board-list)) 20 50 90 50)
                                  )
                    ))]))

(define pass (new button%
                    [parent row1]
                    [label "Terminar turno"]
                    [horiz-margin 20]
                    [callback (lambda (button event)
                                (set! board-list (pass-turn board-list))
                                (draw-label (current-player-name (get-players board-list)) 20 50 90 50)
                                )]))


(define (load-bitmap path factor)
  (scale factor (bitmap/file path))
)


(define (draw-image path x y factor)
  (render-image (load-bitmap path factor) drawer x y)
)


(define (draw-text text tamaño-de-fuente color x y)
  (render-image (text/font
   text
   tamaño-de-fuente
   color
   #f
   "roman"
   "italic"
   "bold"
   #f
  )
  drawer x y)
)

(define (check-quotes jugadores)
  (cond
    ((empty? jugadores) #t)
    ((not(symbol? (car jugadores))) #f)
    (else (check-quotes (cdr jugadores)))
  )
)


(define (validate-list jugadores)
  (cond
    ([not (list? jugadores)] #f)
    ((> (length jugadores) 3) #f)
    ((empty? jugadores) #f)
    ((not [check-quotes jugadores]) #f)
    (else #t)
  )
)


(define (BCEj jugadores)
  (cond
   ((validate-list jugadores); comprueba que la lista de jugadores tenga un formato válido para el juego.

        ;(prepara las variables para comenzar el juego)
        (set! board-list (start-game jugadores))
        (send my-frame show #t)
        (set! player-list (append '(crupier) jugadores))
        (set! positions (get-n-data positions (+ (length jugadores) 1)))
        (sleep/yield 1)
        (send drawer set-font (make-object font% font-size 'default))
        (draw-everyone-cards (get-players board-list) #f)
        (draw-image "Deck/52.png" 687 11 0.38001)
        (draw-image "Deck/52.png" 690 11 0.38001)
        (draw-image "Deck/52.png" 693 11 0.38001)
        (draw-image "Deck/52.png" 696 11 0.38001)
        (draw-image "Deck/52.png" 700 11 0.38001)
        (draw-text (number->string (length (get-deck board-list))) 40 "black" 720 40)
        (draw-players-names jugadores (cdr positions))
        (draw-label (car jugadores) 20 50 90 50)
        
   )(else "lista de nombres no es válida") ; si no es válido, devuelve un mensaje al usuario
  )
)


(define (draw-players-names jugadores posiciones)
  (cond
    ((empty? jugadores) '())
    (else
     (draw-text (symbol->string (car jugadores)) 30 "white" (caar posiciones) (+ 120 (cadar posiciones)))
     (draw-players-names (cdr jugadores) (cdr posiciones))
    )
  )
)

(define (draw-label nombre-del-jugador x y ancho altura)
  (render-image (rectangle ancho altura "solid" "dark green") drawer x y)
  (draw-text (symbol->string nombre-del-jugador) 20 "black" x y)
)

(define (draw-cards lista-de-cartas X Y)
  (cond
    ((empty? lista-de-cartas) '())
    (else (draw-image (create-path (car lista-de-cartas)) X Y 0.38)
          (draw-cards (cdr lista-de-cartas) (+ X 15) Y)
     )
   )
)


(define (draw-everyone-cards jugadores terminado)
(cond
    ((empty? jugadores) '())
    (else (draw-all-cards (current-player-name jugadores) (current-player-cards jugadores) terminado)
          (draw-everyone-cards (cdr jugadores) terminado)
     )
  )
)

(define (draw-all-cards nombre-del-jugador cartas-del-jugador terminado)
  (cond
    ((and (equal? nombre-del-jugador 'crupier) (not terminado))
     (draw-cards
      (append '(52) (cdr cartas-del-jugador))
      (car (get-coords 'crupier player-list positions))
      (cadr (get-coords 'crupier player-list positions)))
    )
    (else
     (draw-cards cartas-del-jugador
                (car (get-coords nombre-del-jugador player-list positions))
                (cadr (get-coords nombre-del-jugador player-list positions)))
    )
  )
)

(define (get-coords jugador nombres-de-jugadores lista-de-coordenadas)
  (cond
    ((empty? nombres-de-jugadores) '())
    ((equal? jugador (car nombres-de-jugadores)) (car lista-de-coordenadas))
    (else (get-coords jugador (cdr nombres-de-jugadores) (cdr lista-de-coordenadas)))
  )
)


(define (create-path n)
  (string-append "Deck/" (number->string n) ".png"))
(define (draw-table table x y)
  (cond
    ((empty? table) '())
    (else
       (draw-text (symbol->string (caar table)) font-size "white" x y)
       (draw-text (number->string (cadar table)) font-size "white" (+ x 150) y)
       (draw-table (cdr table) x (+ font-size y))
     )
   )
)

(define (show-scores)
  (send my-frame2 show #t)
  (sleep/yield 1)
  (draw-image "scoretable.png" 0 0 1)
  (draw-table score-table 250 90)
)

; Llamada a la función BCEj con una lista de jugadores
(BCEj '(jugador1 jugador2 jugador3))

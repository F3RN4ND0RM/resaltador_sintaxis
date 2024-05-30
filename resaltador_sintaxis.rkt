#lang racket

(require srfi/13)  ; Para funciones adicionales de manipulación de cadenas

;; Funciones auxiliares
(define (es-espacio? char)
  (or (char-whitespace? char)
      (char=? char #\tab)
      (char=? char #\newline)
      (char=? char #\return)))

(define (es-puntuacion? char)
  (member char '(#\. #\, #\! #\? #\; #\: #\( #\) #\[ #\] #\{ #\} #\")))

(define (es-operador? char)
  (member char '(#\+ #\- #\* #\/ #\^ #\> #\< #\=)))

(define (es-numero? str)
  (andmap char-numeric? (string->list str)))

;; Función de tokenización
(define (tokenizar str)
  (define len (string-length str))
  (define (loop pos acc)
    (if (>= pos len)
        (reverse acc)
        (let ((char (string-ref str pos)))
          (cond
            ((char=? char #\newline)
             (loop (add1 pos) (cons (list 'newline "<br>") acc)))
            ((es-espacio? char)
             (loop (add1 pos) acc))
            ((es-puntuacion? char)
             (loop (add1 pos) (cons (list 'puntuacion (string char)) acc)))
            ((es-operador? char)
             (loop (add1 pos) (cons (list 'operador (string char)) acc)))
            (else
             (define-values (token next-pos)
               (extract-token str pos len))
             (loop next-pos (cons token acc)))))))
  (loop 0 '()))

;; Función para extraer tokens
(define (extract-token str start len)
  (let loop ((pos start) (token ""))
    (if (or (>= pos len)
            (es-espacio? (string-ref str pos))
            (es-puntuacion? (string-ref str pos))
            (es-operador? (string-ref str pos)))
        (values (if (es-numero? token) (list 'numero token) (list 'palabra token)) pos)
        (loop (add1 pos) (string-append token (string (string-ref str pos)))))))

;; Función para leer el contenido de un archivo
(define (leer-archivo file-path)
  (with-input-from-file file-path
    (lambda ()
      (port->string (current-input-port)))))

;; Función para generar HTML con colores
(define (generar-html tokens)
  (define (token-a-html token)
    (match token
      [(list 'palabra "function") (format "<span style='color:purple;'>~a</span>" "function")]
      [(list 'palabra "if") (format "<span style='color:purple;'>~a</span>" "if")]
      [(list 'palabra "else") (format "<span style='color:purple;'>~a</span>" "else")]
      [(list 'palabra "var") (format "<span style='color:purple;'>~a</span>" "var")]
      [(list 'palabra "return") (format "<span style='color:purple;'>~a</span>" "return")]
      [(list 'palabra "log") (format "<span style='color:purple;'>~a</span>" "log")]
      [(list 'palabra t) (format "<span style='color:blue;'>~a</span>" t)]
      [(list 'puntuacion t) (format "<span style='color:red;'>~a</span>" t)]
      [(list 'operador t) (format "<span style='color:green;'>~a</span>" t)]
      [(list 'newline t) t]
      [(list 'numero t) (format "<span style='color:orange;'>~a</span>" t)]))
  (define cuerpo (string-join (map token-a-html tokens) " "))
  (format "<html><body>~a</body></html>" cuerpo))

;; Ejemplo de uso: leyendo y tokenizando un archivo
(define ruta-archivo "codigo_fuente.txt")
(define contenido (leer-archivo ruta-archivo))
(define tokens (tokenizar contenido))
(define html (generar-html tokens))

;; Guardar el resultado HTML en un archivo
(define (guardar-html ruta contenido-html)
  (with-output-to-file ruta
    (lambda ()
      (display contenido-html))
    #:exists 'replace))

(define ruta-salida "salida.html")
(guardar-html ruta-salida html)
(displayln "HTML generado y guardado en salida.html")



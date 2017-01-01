;; debug



(define debug-mode #f) ;; global debug-mode definition (you can reset it in the function to debug to avoid a lot of information on display) 

(define debug-mode-save debug-mode)

(define (debug-mode-on)
  (set! debug-mode-save debug-mode)
  (set! debug-mode #t))
    
(define (debug-mode-reload)
  (set! debug-mode debug-mode-save))


;; (debug block) or (nodebug block)

(define-syntax debug
  (syntax-rules ()
    
    ((_ instruction)
     (begin
       (debug-mode-on)
       (when debug-mode
	     instruction)
       (debug-mode-reload)))
  
    ((_ instruction ...)
     (begin
       (debug-mode-on)
       (when debug-mode
	     instruction
	     ...)
       (debug-mode-reload)))))


(define-syntax nodebug
  (syntax-rules ()

    ((_ instruction ...)
     (when debug-mode
	   instruction
	   ...))
    
    ((_ instruction)
     (when debug-mode
	   instruction))))

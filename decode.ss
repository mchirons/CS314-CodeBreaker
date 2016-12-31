; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2016                              *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ctv", "vtc",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
;;(load "test-dictionary.ss")

(load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

#|
(define decode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
     (if (null? w) 
     	'()
     	(cons (vtc (modulo (+ (ctv (car w)) n) 26)) ((encode-n n) (cdr w)))
     )
      )))
|# 

(define decode-p;;this encoder is supposed to be the output of "encode-n"
  (lambda (p decoder)
    	(map decoder p)
    ))
     
(define incrementCount
	(lambda (w)
		
		(if (spell-checker w)
			1
			0
		)
		
	)
)
      
(define countSpelling
	(lambda (p) 
		
		(reduce + (map incrementCount p) 0)
	)
)
      
(define testN
	(lambda (n p)
		(if (= n 26) 
			'()
			(cons (countSpelling (decode-p p (encode-n n))) (testN (+ n 1) p))
		)
	)
)

(define getN
	(lambda (p)
		
		(-  (getIndex (reduce max (testN 0 p) 0) (testN 0 p)) 1)
		
	)
)

(define getIndex
	(lambda (n l)
		(cond
			((empty? l) 0)
			((= n (car l)) 1)
			(else (+ 1 (getIndex n (cdr l))))
		)
	)
)

(define add1
	(lambda (x)
		(+ x 1)
	)
)


(define (merge-list lst1 lst2)
  (cond ((null? lst1) lst2)    
        ((null? lst2) lst1)    
        (else (cons (car lst1) 
                    (merge-list lst2 (cdr lst1)))))) 

(define combineWords
	(lambda (p)
		(if (null? p)
			'()
			(merge-list (car p) (combineWords (cdr p)) )
		)
		
	)
)

(define countChar 
	(lambda	(n p)
		(let ((counts (make-hash)))
			(let loop ((p p))
				(cond
					((null? p)
						(hash->list counts)
					)
					(else 
						(hash-update! counts (car p) add1 (lambda () 0))
						(loop (cdr p))
					)
				)
			)
		)
	)		
)

(define maxChar
	(lambda (l)
		(if (null? l)
			#f
			(let loop ((l (cdr l))
					(maxval (cdr(car l)))
					(maxchar (car(car l))))
				(cond 
					((null? l) maxchar)
					((> (cdr (car l)) maxval)
						(loop (cdr l) (cdr(car l)) (car(car l))))

					(else 
						(loop (cdr l) maxval maxchar))			
				)
			)
		)
	)
)



(define maxVal
	(lambda (l)
		(if (null? l)
			#f
			(let loop ((l (cdr l))
					(maxval (cdr(car l)))
					(maxchar (car(car l))))
				(cond 
					((null? l) maxval)
					((> (cdr (car l)) maxval)
						(loop (cdr l) (cdr(car l)) (car(car l))))
					(else 
						(loop (cdr l) maxval maxchar))			
				)
			)
		)
	)
)

(define maxCharList
	(lambda (l val)
		
		(cond
			((null? l) '())
			((= val (cdr (car l)))
				(cons (car (car l)) (maxCharList (cdr l) val))
			)
			(else
				(maxCharList (cdr l) val)
			)
		)
	)
)

(define nList
	(lambda (l)
		
		(append (map shift5 l) (map shift20 l) (map shift1 l))
	)
)

(define testNList

	(lambda (l p)
		(if (null? l)
			0
			( let loop ((l (cdr l))
				 (bestN (car l))
				 (bestScore (testSingleN (car l) p)))
				#|
				(begin
					(display "l:\n")
					(display l)
					(display "\n")
					(display "bestN:\n")
					(display bestN)
					(display "\n")
					(display "bestScore:\n")
					(display "\n")
					(display bestScore)
			
				)
				|#
			
				(cond 
					((null? l) bestN)
					((> (testSingleN (car l) p) bestScore) 
					;(display "true\n")
						(loop (cdr l) (car l) (testSingleN (car l) p))
					)
					(else
						;(display "false\n")
						(loop (cdr l) bestN bestScore)
					)
				)
			)
		)
		
	)
)



(define mostFreq
	(lambda (p)
		(ctv (maxChar (countChar 0 p)))
	)
)

(define testSingleN
	(lambda (n p)
		(countSpelling (decode-p p (encode-n n)))
		
	)
)

(define shift5
	(lambda (char)
	
		(+ (- 25 (ctv char)) 5)
		
	)
)

(define shift20
	(lambda (char)
	
		(+ (- 25 (ctv char)) 20)
		
	)
)

(define shift1
	(lambda (char)
		(+ (- 25 (ctv char)) 1)
		
	)
)



(define getFreqN
	(lambda (p)
		
		
			
		(testNList (nList (maxCharList (countChar 0 (combineWords p)) (maxVal (countChar 0 (combineWords p))))) p)
		;;(testNList (nList (maxCharList ((countChar 0 (combineWords p))) (maxVal (countChar 0 (combineWords p))))) (combineWords p))
		;;(maxVal (countChar 0 (combineWords p)))
		;;(countChar 0 (combineWords p))
		
		;;(testNList (nList (maxCharList (countChar 0 (combineWords p)) (maxVal (countChar 0 (combineWords p))))) p) 
		;;(maxCharList (countChar 0 (combineWords p)) (maxVal (countChar 0 (combineWords p))))
		;;(+ (- 25 (testNList (combineWords p))) 5)
	)
)

(define testDecoderA
	(lambda (d n)
		(Code-Breaker  (encode-d d (encode-n n)) (Gen-Decoder-A (car (encode-d d (encode-n n)))))
		
	)
)

(define testDecoderB
	(lambda (d n)
		(Code-Breaker  (encode-d d (encode-n n)) (Gen-Decoder-B (car (encode-d d (encode-n n)))))
		;;(Gen-Decoder-B (car (encode-d d (encode-n 6))))
	)
)


(define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
   (if (member w dictionary)
   		#t 
   		#f
   	)
   ))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input=a word, output=encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
    
     (if (null? w) 
     	'()
     	(cons (vtc (modulo (+ (ctv (car w)) n) 26)) ((encode-n n) (cdr w)))
     )
      )))
 

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (if (null? d)
    		'()
    		(cons (map encoder (car d)) (encode-d (cdr d) encoder))
    	)
    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
     (encode-n (getN p))
    ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    (encode-n (getFreqN p))
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    
     (if (null? d)
     	'()
     	(cons (map decoder (car d)) (Code-Breaker (cdr d) decoder))
     )
     ))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A (car document)))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)

#lang racket
; (animals questions) structure - ( ( animal0 ... animaln ) (q1 A1 B1 K1) ... (qk Ak Bk Kk) ) - x<-Ai, qi x?->yes; x<-Bi qim x?->no; x<-Kim qi x?->idk
(define T '( ( "collibri" "dog" "drake" "parrot" "penguin") ("Does it fly?" ("drake" "parrot") () ("collibri" "dog" "penguin")) ("Does it have wings?" ("collibri" "drake" "parrot" "penguin") ("dog") ()) ("Is it a quadruped?" ( "dog" ) ("collibri" "drake" "parrot" "penguin"  ) ())  ))

;no lies:
;example1 |C|>0, (car c) == x => (main T)->no->yes
;example2 |C|>0, (car c) != x, x<-C => (main T)->yes->yes->no->parrot
;example3 |C|>0, (car c) != x, {x}C={}, x<-A => (main T)->yes->yes->no->penguin
;example4 |C|>0, (car c) != x, {x}C={}, {x}A={} => (main T)->yes->yes->no->ostrich
;example5 C={}, x<-A => (main T)->yes->no->penguin
;example6 C={}, {x}A={} => (main T)->yes->no->ostrich
;with lies:
;example7 |C|>0, (car c) != x, {x}C={}, x<-A => (main T)->no->no->parrot
;example8 C={}, x<-A => (main T)->yes->no->drake

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-q car)
(define get-A cadr)
(define get-B caddr)
(define get-K cadddr)

;assuming G is non-empty and is the second part of the (animals question structure)
;assumes the questionq is 100% guaranteed to be in G
;returns that question with its' sets
(define (question-info q G)
  (cond
    ((string<? (get-q (car G)) q) (question-info q (cdr G)))
    (else (car G))
    )
  )

;should be known-animal-l? really
;is the animal a part of the list G
(define (known-animal? a G)
  (cond
    ((null? G) #f)
    ((string<? (car G) a) (known-animal? a (cdr G)))
    ((string=? a (car G)) #t)
    (else #f)
    )
  )

;should be known-question-l really?
;is the question q part of the list q
(define (known-question? q G)
  
  (cond
    ((null? G) #f)
    ((string<? (get-q (car G)) q) (known-question? q (cdr G)))
    ((string=? q (get-q (car G))) #t)
    (else #f)
    )
  )

;adds a new question q into an (animals questions) structure and makes his K set = all animals set
(define (add-question q G)
  (define T (car G))
  (define (helper G)
  (cond
    ((null? G) (list (list q '() '() T)))
    ((string<? (get-q (car G)) q) (cons (car G) (helper (cdr G)))) 
    ((string=? (get-q (car G)) q) G)
    (else (cons (list q '() '() T) G))
    )
    )
  (cons T (helper (cdr G)))
  )
;adds an animal a in a list G
(define (add-animal-l a G)
   (cond
    ((null? G) (list a))
    ((string<? (car G) a) (cons (car G) (add-animal-l a (cdr G)))) 
    ((string=? (car G) a) G)
    (else (cons a G))
    )
  )
;removes an animal a from a list G
(define (remove-animal-l a G)
  (cond
    ((null? G) '())
    ((string<? (car G) a) (cons (car G) (remove-animal-l a (cdr G)))) 
    ((string=? (car G) a) (cdr G))
    (else G)
    )
  )
;adds an animal a into an (animals questions) structure and modifies each question's K set by adding the new animal in it
(define (add-animal a G)
  (define (helper G);adds a in every question's K set
    (if (null? G) '()
        (cons (list (get-q (car G)) (get-A (car G)) (get-B (car G)) (add-animal-l a (get-K (car G)))) (helper (cdr G)))
        )
    )
  (cons (add-animal-l a (car G)) (helper (cdr G)))
  )


;let a be an animal and q be a question into the (animals questions) structure
;then moves a from q's K set to either q's A set if answer==#t or q's B set it answer==#f
(define (associate-animal-question a answer q G)
  (define (helper G)
  (cond
    ((string<? (get-q (car G)) q) (cons (car G) (helper (cdr G))))
    ((string=? (get-q (car G)) q) (cons (if answer    (list q (add-animal-l a (get-A (car G))) (get-B (car G)) (remove-animal-l a (get-K (car G))) )    (list q (get-A (car G)) (add-animal-l a (get-B (car G))) (remove-animal-l a (get-K (car G))))) (cdr G)))
    (else (error "No such question"))
    )
    )
  (cons (car G) (helper (cdr G)))
  )

;let path be represented in the following way
;((q1 answer1) ... (qn answern))
(define (update-path a path G)
  (define (helper path G)
    (if (null? path) G
        (helper (cdr path) (associate-animal-question a (cadar path) (get-q (car path)) G))
        )
    )
  (helper path G)
  )
;unions two set of animals (ordered)
(define (union-ordered A B)
  (cond
    ((null? A) B)
    ((null? B) A)
    ((string<? (car A) (car B)) (cons (car A) (union-ordered (cdr A) B)))
    ((string>? (car A) (car B)) (cons (car B) (union-ordered B (cdr A))))
    (else (cons (car A) (union-ordered (cdr A) (cdr B))))
    )
  )
;intersects two sers of animals (ordered)
(define (intersection-ordered A B)
  (cond
    ((null? A) '())
    ((null? B) '())
    ((string<? (car A) (car B)) (intersection-ordered (cdr A) B))
    ((string>? (car A) (car B)) (intersection-ordered A (cdr B)))
    (else (cons (car A) (intersection-ordered (cdr A) (cdr B))))
    )
  )


;insert q in a sorted list T
;first sorted by |K|, 2nd by ||A|-|B||
(define (insert-sorted q T)
  (cond
    ((null? T) (list q))
    ((< (length (get-K (car T))) (length (get-K q)))(cons (car T) (insert-sorted q (cdr T))))
    ((eq? (length (get-K (car T))) (length (get-K q))) (if (> (abs (- (length (get-A (car T))) (length (get-B (car T))))) (abs (- (length (get-A q)) (length (get-B q))))) (cons q T) (cons (car T) (insert-sorted q (cdr T)))))
    (else (cons q T))
    )
  )

;given a question q = ("name" qA qB qK) and A and K produces q' = ("name" AqA AqB KUqK)
(define (transform-question A K q)
  (list (get-q q) (intersection-ordered A (get-A q)) (intersection-ordered A (get-B q)) (union-ordered K (get-K q)))
  )

;transforms the question set by intersecting its' A and B set with A(the input A set) and uniting its' K set and the input K set
;then makes an sorted list T
;first sorted by |K|, 2nd by ||A|-|B||
;after that removes all irrelevant questions (look at choose descr)
(define (step A K G)
  ;transforms every question in G, then inserts it into a (1st: |K|, 2nd: ||A|-|B||) sorted list
  (define (half-step G T)
    (if (null? G) T
        (half-step (cdr G) (insert-sorted (transform-question A K (car G)) T))
        )
    )
  ;finds the first question for which |qA|<|A|&&|qB|<|A|&&((|qA|,|qB|)!=(0,0)) and returns it with the rest of the list or returns the last element
  (define (choose T)
    (cond
      ((null? (cdr T)) T)
      ((or (and (eq? (length (get-A (car T))) 0) (eq? (length (get-B (car T))) 0)) (eq? (length (get-A (car T))) (length A)) (eq? (length (get-B (car T))) (length A))) (choose (cdr T)))
      (else T)
      )
    )
  (choose (half-step G '()))
  )

;just pushes a back into l
(define (push-back a l)
  (if (null? l) (list a)
      (cons (car l) (push-back a (cdr l)))
      )
  )

;learns to differentiate a from a whole list of animals l
(define (differentiate-betweenL a l G)
  (cond
    ((null? l) '())
    (else (cons (differentiate-between a (car l) G) (differentiate-betweenL a (cdr l) G)))
    )
  )
;learns to differentiate between all the animals from list l1 from the ones from list l2, l1==l2
(define (differentiate-betweenLM l G)
  (define (helper l1 l2)
  (if (null? (remove-animal-l (car l1) l2)) '()
      (append (differentiate-betweenL (car l1) (remove-animal-l (car l1) l2) G) (helper (cdr l1) (remove-animal-l (car l1) l2)))
      )
    )
  (helper l l)
  )
;after given a list produced from (differentiate-betweenL a l) updates the (animals questions) structure with it
(define (update-difference l G)
  (cond
    ((null? l) G)
    ((known-question? (get-q (car l)) (cdr G)) (update-difference (cdr l) (associate-animal-question (caddar l) #f (get-q (car l)) (associate-animal-question (cadar l) #t (get-q (car l)) G))))
    (else (update-difference l (add-question (get-q (car l)) G)))
    )
  )

;checks if you've been lying
(define (did-you-lie a path G)
  (cond
    ((null? path) '())
    ((eq? (cadar path) #t) (if (known-animal? a (get-B (question-info (get-q (car path)) (cdr G)))) (cons (list (get-q (car path)) #f) (did-you-lie a (cdr path) G)) (did-you-lie a (cdr path) G))) ;checks if you lied if you did adds the question and right answer in the return list
    (else (if (known-animal? a (get-A (question-info (get-q (car path)) (cdr G)))) (cons (list (get-q (car path)) #t) (did-you-lie a (cdr path) G)) (did-you-lie a (cdr path) G) ))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Questions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;asks question q, expects yes or no as an answer
(define (ask-question q)
  (display q)
  (display #\return)
  (string=? (read-line (current-input-port)) "yes")
  )
;expects the name of the animal as an answer
(define (give-up)
  (display "I give up. What is the answer?")
  (display #\return)
  (read-line (current-input-port))
  )
;returns #t if the animal was a
(define (animal-is a)
  (if (eq? (car (string->list a)) #\a)
      (display "Is the animal an ")
      (display "Is the animal a ")
      )
  (display a)
  (display "?")
  (display #\return)
  (string=? (read-line (current-input-port)) "yes")
  )
;asks how to differentiate between a and b
;saves the answer into the following sturcture (q a b) : q-question; q a?->yes; q b?->no
(define (differentiate-between a b G)
  (display "How can I differentiate between ")
  (display a)
  (display " and ")
  (display b)
  (display "?")
  (display #\return)
  (let ((p (read-line (current-input-port)))) #| 
    ;if it's a known question
    (if (known-question? p G)
        (let ((qi (question-info p G)))
          (if (or (and (known-animal? a (get-A qi)) (known-animal? b (get-A qi))) (and (known-animal? a (get-B qi)) (known-animal? b (get-B qi)))) (begin (display "That question won't help me.") (display #\return) (differentiate-between a b G)) ;if youdecide to lie
              (begin
                (display "For which, of these two, is the answer yes to this question?")
                (display #\return)
                (if (string=? (read-line (current-input-port)) a) 
                    (if (or (known-animal? a (get-B qi)) (known-animal? b (get-A qi))) (begin (display "Don't bullshit me pls.") (display #\return) (differentiate-between a b G)) ;if you decide to lie
                        (list p a b)
                        )
                    (if (or (known-animal? a (get-B qi)) (known-animal? b (get-A qi))) (begin (display "Don't bullshit me pls.") (display #\return) (differentiate-between a b G)) ;if you deicde to lie
                        (list p b a)
                        )
                    )
                )
                
          )
          )
              (begin |#
                (display "For which, of these two, is the answer yes to this question?")
                (display #\return)
                (if (string=? (read-line (current-input-port)) a) (list p a b)
                    (list p b a)
                    )
                )
      ;  )
   ; )
  )




(define (you-lied l)
  (define (helper l)
    (if (null? l) (display "So I am going to scrap your answers.")
        (begin
          (display "At question ")
          (display (caar l))
          (display " you should have answered ")
          (if (cadar l) (display "yes.")
              (display "no.")
            )
          (display #\return)
          (helper (cdr l))
          (display #\return)
          )
        )
    )
  (display "Seems you lied somewhere along the way.")
  (display #\return)
  (display "More precisely:")
  (display #\return)
  (helper l)
  )

(define (making-a-fool-out-of-me)
  (display "You are making a fool out of me, eh?")
  (display #\return)
  (display "I'll just scrap your answers.")
  (display #\return)
  )

(define (do-you-wish-to-save)
  (display "Do you wish to save your progress to a file?")
  (display #\return)
  (if (string=? (read-line (current-input-port)) "yes")
      (begin
        (display "Input the file name in which you would like to save")
        (display #\return)
        (read-line (current-input-port))
        )
      #f
      )
  )

(define (do-you-want-to-play-another-game)
  (display "Do you want to play another game?")
  (display #\return)
  (string=? (read-line (current-input-port)) "yes")
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;THE BIG MESS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start-game G tryhard)
  ;decision making
  (define (resolve-animals A path)
    (if (animal-is (car A)) G ;is the first element of A the animal? if yes => OK I win
      (let ((p (give-up))) ;if no, give up and find out the animal => is it from A?
        (if (string=? p (car A)) (begin (making-a-fool-out-of-me) G) ; so the answer is (car A) but you say it's not then you give the same answer as (car A)? I think not
          (if (known-animal? p (cdr A)) (if tryhard (update-difference (differentiate-betweenLM A (cdr G)) G) (update-difference (differentiate-betweenL p (cdr A) (cdr G)) G) ) ;if it is from A just learn to differentiate it from the others
              ;if it isn't check whether it's a known animal
              (if (known-animal? p (car G))
                  ;if it is known check if the user lied, if he didn't then just update the route, then find how to differentiate it from the others
                  (let ((r (did-you-lie p path G)))
                    (if (null? r) (if tryhard (update-difference (differentiate-betweenLM (add-animal-l p A) (cdr G)) (update-path p path G)) (update-difference (differentiate-betweenL p (remove-animal-l p A) (cdr G)) (update-path p path G)) )
                        (begin (you-lied r) G)
                        )
                    )
                  ;if it is not then add it then do the same
                  (if tryhard (update-difference (differentiate-betweenLM (add-animal-l p A) (cdr G)) (update-path p path (add-animal p G))) (update-difference (differentiate-betweenL p (remove-animal-l p A) (cdr G)) (update-path p path (add-animal p G))) )
                  )
              )
          )
        )
      )
    )
  ;main loop
  (define (helper A K path T)
    (cond
      ;(null? A) check also whether you lied
      ((null? A) (let ((p (give-up))) (if (known-animal? p (car G)) (let ((r (did-you-lie p path G))) (if (null? r) (update-path p path G) (begin (you-lied r) G) )) (update-path p path (add-animal p G)))));no animals in A=>give up, if the animal is not in the database add it, use the answered questions to add it in the question DB
      ((or (null? T) (null? (cdr A))) (resolve-animals A path)) ;if no reamining questions or A is a singleton=>try to get to a conclusion with the last question
      ((null? (cdr T)) (if (ask-question (get-q (car T)))  (helper (intersection-ordered (get-A (car T)) A) (union-ordered (get-K (car T)) K) (push-back (list (get-q (car T)) #t) path) (cdr T)) (helper (intersection-ordered (get-B (car T)) A) (union-ordered (get-K (car T)) K) (push-back (list (get-q (car T)) #f) path) (cdr T))));if there's one question left, ask it and update the sets+path
      (else (let ((p (step A K T))) (if (ask-question (get-q (car p))) (helper (get-A (car p)) (get-K (car p)) (push-back (list (get-q (car p)) #t) path) (cdr p)) (helper (get-B (car p)) (get-K (car p)) (push-back (list (get-q (car p)) #f) path) (cdr p)))) );just continue step by step
      )
    )
  (helper (car G) '() '() (cdr G))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Some I/O;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(read-db "a.txt")
(define (read-db a)
  (define (remove-end str)
    (if (eq? (string-ref str (- (string-length str) 1)) #\return) (substring str 0 (- (string-length str) 1))
        str
    )
    )
  (define in (open-input-file a))
  (define (start-reading)
    
    (define (start-reading2 t)
      
      (define (start-reading3 t)
        
        (define (read-part s)
          (let ((r (remove-end (read-line in))))
            (cond
              ((eof-object? r) (begin (display "File ended too early, or does not conform to animals save file format.") (close-input-port in)))
              ((string=? r s) '())
              (else (cons r (read-part s)))
              )
            )
          )
        
        (let ((r (remove-end (read-line in))))
          (cond
          ((eof-object? r) (begin (display "File ended too early, or does not conform to animals save file format.") (close-input-port in)))
          ((string=? r "</>") (begin (close-input-port in) t))
          ((string=? r  "<Q>") (start-reading3 (push-back (list (remove-end (read-line in)) (read-part "</qA>") (read-part  "</qB>") (read-part "</qK>")) t)))
          (else (begin (display "File does not conform to animals save file format.") (close-input-port in)))
          )
          )
        );;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;start-reading3 end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
      (let ((r (remove-end (read-line in))))
        (cond
          ((eof-object? r) (list t '()))
          ((string=? r "</A>") (cons t (start-reading3 '())))
          (else (start-reading2 (add-animal-l r t)))
          )
        )
      );;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;start-reading2 end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (let ((r (remove-end (read-line in))))
      (cond
      ((eof-object? r) (begin (display "File is empty.") (close-input-port in)))
      ((string=? r "<A>") (start-reading2 '()) )
      (else (begin (display "File does not conform to animals save file format.") (close-input-port in)))
      )
      )
    );;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;start-reading end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (start-reading)
  )

(define (write-db a G)
  (define in (open-output-file a #:exists 'truncate))
  ;write the animal part ...</A>
  (define (write-animals G)
    (if (null? G) (write-string "</A>\n" in)
        (begin (write-string (car G) in) (write-string "\n" in) (write-animals (cdr G)))
        )
    )
  ;write the questions part ...</>
  (define (write-questions G)

    (define (write-part s l)
      (if (null? l) (write-string s in)
          (begin (write-string (car l) in) (write-string "\n" in) (write-part s (cdr l)))
          )
      )
    (if (null? G) (write-string "</>" in)
        (begin (write-string "<Q>\n" in) (write-string (get-q (car G)) in) (write-string "\n" in) (write-part "</qA>\n" (get-A (car G))) (write-part "</qB>\n" (get-B (car G))) (write-part "</qK>\n" (get-K (car G))) (write-questions (cdr G)))
        )
    )
  (write-string "<A>\n" in)
  (write-animals (car G))
  (write-questions (cdr G))
  (close-output-port in)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAIN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main T tryhard)
  (define (helper G)
    (let ((P (start-game G tryhard)) (ws (do-you-wish-to-save)) (pa (do-you-want-to-play-another-game)) )
      (if (eq? ws #f) (void)
        (write-db ws P)
        )
      (if (eq? pa #t) (helper P)
          (display "Cya then.")
          )
    )
    )
  (helper T)
  )

;;tests:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test func
(define (read-bs2 a)
  (define in (open-input-file a))
  (define (helper r t)
    (if (eof-object? r) (begin (close-input-port in) t)
        (helper (read-line in) (cons r t))
    )
    )
  (helper (read-line in) '())
  )
;test func
(define (write-bs2 a t)
  (define in (open-output-file a #:exists 'truncate))
  (write-string t in)
  (close-output-port in)
  )

(main T #t)
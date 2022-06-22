#lang scheme

((λ ()
  (define member?
    (λ (a lat)
      (cond
        ((null? lat) #f)
        (else (or (eq? a (car lat))
                  (member? a (cdr lat)))))))
  
   (member? 'sardines '(Italian sardines spaghetti parsley))
   ))

((λ ()
   (define is-first?
     (λ (lat a)
       (cond
         ((null? lat) #f)
         (else (or (eq? (car lat) a)
                   (is-first? (cdr lat) a))))))
   
   (define two-in-a-row?
     (λ (lat)
       (cond
         ((null? lat) #f)
         (else (or (is-first? (cdr lat) (car lat))
                (two-in-a-row? (cdr lat)))))))
   (println (two-in-a-row? '(a b a c)))
   (println (two-in-a-row? '(b a b a c)))
   (println (two-in-a-row? '(a b c d)))
   ))

((λ ()
   (define two-in-a-row?
     (λ (lat)
       (cond
         ((null? lat) #f)
         (else
          (is-first-b? (car lat) (cdr lat))))))

   (define is-first-b?
     (λ (a lat)
       (cond ((null? lat) #f)
             (else (or (eq? a (car lat))
                       (two-in-a-row? lat))))))
   
   (println (two-in-a-row? '(a b a c d)))
   (println (two-in-a-row? '(a a c)))
   (println (two-in-a-row? '(b a b a c)))
   (println (two-in-a-row? '(a b c d)))
   ))
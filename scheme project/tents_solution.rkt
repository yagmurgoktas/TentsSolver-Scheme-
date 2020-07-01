#lang scheme
;STUDENT-NO 2017400018
(define RETURN-FIRST-NOT-FALSE (lambda (fn prm1 prm2 prm3 prm4 prm5 liste)(
                                                                      if(null? liste) #f (
                                                  if (not (eq? (fn prm1 (car liste) prm2 prm3 prm4 prm5 ) #f))
                                                     (fn prm1 (car liste) prm2 prm3 prm4 prm5 )
                                                     (RETURN-FIRST-NOT-FALSE fn prm1 prm2 prm3 prm4 prm5 (cdr liste))))))

(define ADJACENT (lambda (list1 list2)(
                                       if( or (and (eq? (car list1)  (car list2))(eq? (car (cdr list1)) (+ (car (cdr  list2))  1)))
                                              ( or (and (eq? (car list1) (car list2))(eq? (car (cdr  list1)) (- (car (cdr  list2))  1)))
                                                   ( or (and (eq? (car list1) (+ (car list2) 1))(eq? (car (cdr list1)) (car (cdr list2)) ))
                                                        ( or (and (eq?  (car list1) (- (car list2) 1))(eq?  (car (cdr list1)) (car (cdr  list2)) ))
                                                             ( or (and (eq?  (car list1) (+ (car list2) 1))(eq? (car (cdr  list1)) (- (car (cdr  list2))  1)))
                                                                  ( or (and (eq?  (car list1) (+  (car list2) 1))(eq? (car (cdr  list1)) (+ (car (cdr list2))  1)))
                                                                       ( or (and (eq?  (car list1) (-  (car list2) 1))(eq? (car (cdr  list1)) (- (car (cdr  list2))  1)))
                                                                             (and (eq? (car list1) (- (car list2) 1))(eq? (car (cdr  list1)) (+ (car (cdr  list2))  1))))))))))
                                         #t
                                         #f)))
                                         
         
                                                                                                                                                              

(define NEIGHBOR-LIST (lambda (liste)
                                    (cons (list ( car liste) (+ (car ( cdr liste)) 1)) (cons (list (+ ( car liste) 1) (car ( cdr liste))) (cons (list ( car liste) (- (car ( cdr liste)) 1) ) (list (list (- ( car liste) 1) (car ( cdr liste)))))))))

(define ADJACENT-WITH-LIST (lambda (liste listofliste)(
                                                       if(null? listofliste)
                                                         #f
                                                       (if (ADJACENT liste (list (car (car listofliste)) (car (cdr (car listofliste)))))
                                                          #t
                                                          (ADJACENT-WITH-LIST liste (cdr listofliste))))))

(define REPLACE-NTH (lambda (liste i v)(                                      
                              if (eq? i 1)
                                 (cons v (cdr liste))
                                 (cons (car liste) (REPLACE-NTH (cdr liste) (- i 1) v)))))

(define nth (lambda (liste n) (
                               if(null? liste ) 0
              (
                               if (eq? n 0) 0
                              (if (eq? n 1)
                                 (car liste)
                                 (nth (cdr liste) (- n 1)))))))

(define check (lambda (rowlist columnlist)(
                                           if(and (eq? (allzero rowlist) #t) (eq? (allzero columnlist) #t) )
                                             #t
                                             #f)))
                                           
(define allzero (lambda (liste)(
                                if(null? liste)
                                  #t
                                (if(eq? (car liste) 0)
                                  (allzero (cdr liste))
                                  #f))))
                                  
(define sub1 (lambda (liste n) (
                              if (eq? n 1)
                                (cons (- (car liste) 1) (cdr liste))
                                (cons (car liste) (sub1 (cdr liste) (- n 1))))))                                                    

(define deneme (lambda (treelist rowlist columnlist tentlist trees)(
                                         if(null? treelist)
                                                (if(eq? (check rowlist columnlist) #t )
                                           tentlist #f)
                                           (RETURN-FIRST-NOT-FALSE try tentlist treelist rowlist columnlist trees (NEIGHBOR-LIST (car treelist) )))))

(define try ( lambda (tentlist liste treelist rowlist columnlist trees)(
                                      if (or (eq? (nth rowlist (car liste)) 0) (or (eq? (nth columnlist (car (cdr liste))) 0) (or  (ADJACENT-WITH-LIST liste tentlist) (member liste trees) ) ))
                                         #f                                                                            
                                        (deneme (cdr treelist) (sub1 rowlist (car liste)) (sub1 columnlist (car (cdr liste)) )(cons liste tentlist) trees))))
                          
(define TENTS-SOLUTION (lambda (liste)
                         (deneme (car (cdr (cdr liste))) (car liste) (car (cdr liste)) '()  (car (cdr (cdr liste))) ) ))





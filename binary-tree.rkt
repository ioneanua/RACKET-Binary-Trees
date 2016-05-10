#lang racket

(require racket/include)
(include "binary-tree-test.rkt")

;; TASK pregatitor
(define binary-search-tree '(9 (3 (2 (1 () ()) () ) (6 (5 () ()) (8 (7 () ()) ())) ) (12 (11 () ()) (15 (13 () ()) (21 () ()))))  )
(define empty-tree '()) 

;init-node -> primește o valoare și returnează o frunză ce conține acea valoare
;--------------------------------------------------------------------------------------------------------
(define init-node
  (λ (value)
    (list value '() '())
    )
  )

;make-node -> returnează un nod, având componentele specificate;
;--------------------------------------------------------------------------------------------------------
(define make-node
  (λ (left right value)
    (list value left right)))

;is-leaf? -> verifică dacă un nod este frunză;
;--------------------------------------------------------------------------------------------------------
(define is-leaf?
  (λ (node)
    (if (is-empty? node) #f 
        (if (and  (null? (car (cdr node))) (null? (car (cdr (cdr node)))))  #t #f ))))

;get-value -> returnează valoarea reținută într-un nod;
;--------------------------------------------------------------------------------------------------------
(define get-value
  (λ (node)
    (if (is-empty? node) '() 
        (car node))))

;get-left -> returnează subarborele stâng al unui arbore
;--------------------------------------------------------------------------------------------------------
(define get-left
  (λ (node)
    (if (is-empty? node) '() (car (cdr node)))
    )
  )

;get-right -> returnează subarborele drept al unui arbore;
;--------------------------------------------------------------------------------------------------------
(define get-right
  (λ (node)
    (if (is-empty? node) '()
        (car (cdr (cdr node))) )
    )
  )

;is-node? -> verifică dacă parametrul primit respectă reprezentarea aleasă pentru un nod;
;--------------------------------------------------------------------------------------------------------
(define is-node?
  (λ (node)
    (if (not (list? node)) #t 
       (if (and (list? (car (cdr node))) (list? (car (cdr (cdr node))))) #t #f))
    )
 )


;is-empty? -> verifică dacă arborele primit ca parametru este vid;
;--------------------------------------------------------------------------------------------------------
(define is-empty?
  (λ (tree)
    (if (null? tree) #t #f)))

;has-left? -> verifică dacă arborele conține un subarbore stâng nevid;
;-------------------------------------------------------------------------------------------------------- 
(define has-left?
  (λ (tree)
    (if (is-empty? tree) #f
        (not (null? (car (cdr tree))))) 
    )
  )

;has-right? -> verifică dacă arborele conține un subarbore drept nevid;
;--------------------------------------------------------------------------------------------------------
(define has-right?
  (λ (tree)
    (if (is-empty? tree) #f 
        (not (null? (car(cdr (cdr tree))))))
    )
  )

;minimum -> determină valoarea minimă din arborele binar de căutare;
;--------------------------------------------------------------------------------------------------------
(define minimum
  (λ (tree)
    (if (is-empty? tree) '()
        (if (has-left? tree)
            (minimum (get-left tree))
            (car tree) ) )
    )
  )

;maximum -> determină valoarea maximă din arborele binar de căutare;
;--------------------------------------------------------------------------------------------------------
(define maximum
  (λ (tree)
    (if (has-right? tree)
        (maximum (get-right tree))
        (car tree)
        )
    )
  )

;height -> întoarce înălțimea unui arbore binar;
;--------------------------------------------------------------------------------------------------------
(define height
  (λ (tree)
    (if (null? tree) 0
        (+ 1 (max (height (get-left tree)) (height (get-right tree))))
        )
    )
  )

;inorder -> formează o listă cu valorile nodurilor parcurse în inordine;
;--------------------------------------------------------------------------------------------------------
(define inorder
  (λ (tree) 
    (if (null? tree) '() 
        (append (inorder (get-left tree)) (list (car tree)) (inorder (get-right tree)))
        )
    )
  )

;preorder -> formează o listă cu valorile nodurilor parcurse în inordine;
;--------------------------------------------------------------------------------------------------------
(define preorder
  (λ (tree)
    (if (null? tree) '()
        (append (list (car tree)) (preorder (get-left tree)) (preorder (get-right tree)))
        )
    )
  )

;postorder -> formează o listă cu valorile nodurilor parcurse în postordine;
;--------------------------------------------------------------------------------------------------------
(define postorder
  (λ (tree)
    (if (null? tree) '()
        (append (postorder (get-left tree)) (postorder (get-right tree)) (list (car tree)))
        )
    )
  )

;successor -> primește un arbore binar de căutare și o valoare și determină succesorul valorii;
;--------------------------------------------------------------------------------------------------------
(define successor
  (λ (tree value)
    (cond
      [(> value (maximum tree)) (maximum tree)]
      [(contains tree (+ 1 value)) (+ 1 value)]
      (else (successor tree (+ 1 value)))
      )
    )
  )

;predecessor -> primește un arbore binar de căutare și o valoare și are ca rezultat predecesorul valorii;
;--------------------------------------------------------------------------------------------------------
(define predecessor
  (λ (tree value)
    (cond
      [(< value (minimum tree)) (minimum tree)]
      [(contains tree (- value 1)) (- value 1)]
      (else (predecessor tree (- value 1)))
      )
    )
  )


;(define binary-search-tree empty-tree)


;;Task 1

;insert -> primește un arbore binar de căutare și o valoare pe care o inserează în arbore, dacă nu există, returnând arborele rezultat;
;- daca arborele primit ca parametru este null ; creem noi un arbore cu valoare primita ca parametru
;daca valoarea este mai mica sau mai mare decat root introducem recursiv si balansand totodata valaorea pe subarborele potrivit
;--------------------------------------------------------------------------------------------------------------------------------------
(define insert
  (λ (tree value) 
    (cond
      [(null? tree) (list value '() '()) ]
      [(< value (get-value tree)) (balance (make-node (insert (get-left tree) value) (get-right tree) (get-value tree)))]
      [(> value (get-value tree)) (balance (make-node (get-left tree) (insert (get-right tree) value) (get-value tree)))]
      [else (balance tree)]
      )
    )
  )

;balance -> primește ca parametru un arbore binar de căutare neechilibrat și are ca rezultat un arbore AVL;
;--------------------------------------------------------------------------------------------------------------------------------------
(define rotate-left
  (λ (tree)
  (cond ((null? tree) tree)
        (else (make-node (get-left (get-left tree))
                         (make-node (get-right (get-left tree)) (get-right tree) (get-value tree))
                         (get-value (get-left tree))))
        )
  )
)

(define rotate-right
  (λ (tree)
  (cond ((null? tree) tree)
        (else (make-node (make-node (get-left tree) (get-left (get-right tree)) (get-value tree))
                         (get-right (get-right tree))
                         (get-value (get-right tree))
                         )
              )
        )
    )
)

(define rotate-left-right
  (λ (tree)
   (rotate-left (make-node (rotate-right (get-left tree)) (get-right tree) (get-value tree)))
   
  )
)

(define rotate-right-left
   (λ (tree)
   (rotate-right (make-node (get-left tree) (rotate-left (get-right tree)) (get-value tree)))
   )
)


;-am avut prima data de rotatii stanga , dreapta , stanga-dreapta si dreapta-stanga
;am definit diferenta dintre inaltimea subarborelui drept si subarborelui stang
;daca aceasta este mai mare decat 1 si daca diferenta aplicata pe subarborele drept este mai mare decat 0 atunci vom roti arborele
;dreapta -stanga dupa care drepta ;
;iar daca diferenta este mai mica decat -1 facem invers
(define balance
  (λ (tree)
  (define (difference tree) (- (height (get-right tree)) (height (get-left tree))))
  (let ((diff (difference tree))) 
    (cond [(> diff 1) 
           (if (< (difference (get-right tree)) 0) (rotate-right-left tree)
               (rotate-right tree))]
          [(< diff -1)
           (if (> (difference (get-left tree)) 0) (rotate-left-right tree) (rotate-left tree))]
          [else tree]
          )
    )
  )  
)




;union -> primește doi arbori binari de căutare și returneaza reuniunea lor, tot sub formă de arbore binar de căutare;
;--------------------------------------------------------------------------------------------------------------------------------------
(define union
  (λ (tree1 tree2)
    (cond
      [(null? tree2) tree1]
      [(contains tree1 (get-value tree2)) (union tree1 (remove tree2 (car tree2)))]
      [else (insert (union tree1 (remove tree2 (car tree2))) (car tree2))] 
      )
    )
  )

;intersection -> primește doi arbori binari de căutare și returnează intersecția lor, rezultatul fiind reprezentat sub forma unui arbore binar de căutare echilibrat
;- am definit o functie separat myIntersection care primeste pe langa cei doi tree1 si tree2 inca un temp ; vedem daca un element din tree1 se afla in tree2
; si il adaugam in temp cu insert care face automat balance
;--------------------------------------------------------------------------------------------------------------------------------------
(define intersection
  (λ (tree1 tree2)
    (myIntersection tree1 tree2 '())))

(define myIntersection
  (λ (tree1 tree2 temp)
    (cond
      [(or (null? tree1) (null? tree2)) temp]
      [(contains tree2 (car tree1)) (myIntersection   (remove tree1 (car tree1)) tree2 (insert temp (car tree1)))]
      [else  (myIntersection (remove tree1 (car tree1)) tree2 temp) ]
      )
    )
  )
  

;complements -> primește doi arbori binari de căutare și returnează diferența lor, rezultatul fiind reprezentat sub forma unui arbore binar de căutare echilibrat;
;am definit o fucntie separat myComplements care, in afara de tree1 si tree2 primeste si un temp , vedem daca un element din primul tree se afla in al doilea
; si daca se afla trecem ami departe pana gasim un element care nu se afla si il adaugam in temp
;--------------------------------------------------------------------------------------------------------------------------------------
(define complements
  (λ (tree1 tree2)
    (myComplements tree1 tree2 '())
    )
  )

(define myComplements
  (λ (tree1 tree2 temp)
    (cond
      [(or (null? tree1) (null? tree2)) temp]
      [(not (contains tree2 (car tree1))) (myComplements (remove tree1 (car tree1)) tree2 (insert temp (car tree1)))]
      [else  (myComplements (remove tree1 (car tree1)) tree2 temp) ]
      )
    )
  )

;contains -> primește un arbore de căutare și o valoare și verifică dacă valoarea există în arbore;
;-primim ca argument un tree si o valoare , cautam recursiv daca valoarea este continuta in tree si returnam true daca
;exista , altfel false
;--------------------------------------------------------------------------------------------------------------------------------------
(define contains
  (λ (tree value)
    (cond
      [(null? tree) #f]
      [(equal? value (get-value tree)) #t]
      [(< value (car tree)) (contains (get-left tree) value)]
      [(> value (car tree)) (contains (get-right tree) value)]
      )
    )
  )

;remove -> primește un arbore binar de căutare și o valoare pe care o va șterge din arbore, returnând arborele binar de căutare rezultat;
;- daca tree-ul este null , se va returna tree-ul ; daca valoare care trebuie stearsa este mai mica sau mai mare decat root-ul
;vom construi nodul fara acea valoare stergandu-l recursiv; daca valoare pe care trebuie sa o stergem are doi copii, atunci
; inlocuim acea valoare cu minimum de pe acea ramura si dupa il stergem
;--------------------------------------------------------------------------------------------------------------------------------------
(define remove
  (λ (tree value)
    (cond
      [(null? tree) tree]
      [(< value (car tree)) (make-node (remove (get-left tree) value) (get-right tree) (get-value tree))]
      [(> value (car tree)) (make-node (get-left tree) (remove (get-right tree) value) (get-value tree))]
      [(and (has-left? tree) (has-right? tree)) (balance (make-node (get-left tree)
                                                           (remove (get-right tree) (minimum (get-right tree)))
                                                           (minimum (get-right tree))))]
      [(has-left? tree) (get-left tree)]
      [else (get-right tree)]
      )
    )
  )

;--------------------------------------------------------------------------------------------------------------------------------------

;;Task 2
(define k-subsets
  (λ (set k) '())
  )

(define zig-zag-subsets
  (λ (set) '())
  )

;;BONUS
(define parser
  (λ (expression) empty-tree)
  )

(define evaluate
  (λ (expr-tree) #f)
  )

;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 30 puncte) ;;check-exp
(define functions (list is-node? is-leaf? is-empty? get-value make-node get-right get-left inorder height insert empty-tree)) ;;check-exp
(define tree0 binary-search-tree) ;;check-exp
(check-exp-part 'is-node .037 (is-node? tree0) #t)
(check-exp-part 'is-leaf?1 .037 (is-leaf? tree0) #f)
(check-exp-part 'is-leaf?2 .037 (is-leaf? (init-node 8)) #t)
(check-exp-part 'is-empty?1 .037 (is-empty? tree0) #f)
(check-exp-part 'is-empty?2 .037 (is-empty? empty-tree) #t)
(check-exp-part 'get-value1 .037 (get-value tree0) 9)
(check-exp-part 'get-value2 .037 (get-value (get-left tree0)) 3)
(check-exp-part 'get-value3 .037 (get-value (get-right tree0)) 12)
(check-exp-part 'make-node .037 (make-node (get-left tree0) (get-right tree0) (get-value tree0)) binary-search-tree)
(check-exp-part 'minimum .0833 (minimum tree0) 1)
(check-exp-part 'maximum .0833 (maximum tree0) 21)
(check-exp-part 'height1 .0833 (height tree0) 5)
(check-exp-part 'height2 .0833 (height (get-left (get-left tree0))) 2)
(check-exp-part 'successor1 .055 (successor tree0 9) 11)
(check-exp-part 'successor2 .055 (successor tree0 5) 6)
(check-exp-part 'successor3 .055 (successor tree0 8) 9)
(check-exp-part 'predecessor1 .056 (predecessor tree0 9) 8)
(check-exp-part 'predecessor2 .056 (predecessor tree0 5) 3)
(check-exp-part 'predecessor3 .057 (predecessor tree0 12) 11)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(define A (create-tree '(8 9 10 15 8 5 0 1 4 5 9 7 1 0 151 651 61 45 416 2542 -8 3541 644 2 4 8542 51 142 215) functions)) ;;check-exp
(define B (create-tree '(942 4 54 64 94 25 0 -815 485 251 64 8 10 5 4 644 2 216 2541 5 8 7 5254 2542 214 4511) functions)) ;;check-exp
(define C (create-tree '(8 5 4 1 846 54 0 -5552 4 5 810 42 545 842 54 5488 8755 14 679 25 78 25 955 7891 789 8891 97 54 15 2465 155) functions)) ;;check-exp
(define D (create-tree '(8 9 1 5 9 7 5 9 78 1 5 6 9 89 24 52 95 22 94 6 485 18 6 97 8 100 4 9 655 478 92) functions)) ;;check-exp
(check-exp-part 'check-set1 .04 (test-task1 (create-tree '(8 4 2 1 -5 6 1 8 9 5 3 11 17 10 -6 4 8) functions) functions) result-check-set1)
(check-exp-part 'check-set2 .04 (test-task1 (create-tree '(-9 8 2 1 4 0 9 3 4 2 5 9 11 481 51 35 15 0 4 15 251 6551 12 3 4 7 9) functions) functions) result-check-set2)
(check-exp-part 'check-set3 .04 (test-task1 A functions) result-check-set3)
(check-exp-part 'check-set4 .04 (test-task1 B functions) result-check-set4)
(check-exp-part 'check-set5 .04 (test-task1 C functions) result-check-set5)
(check-exp-part 'union1 .005 (test-task1 (union A B) functions) result-union1)
(check-exp-part 'union2 .005 (test-task1 (union C D) functions) result-union2)
(check-exp-part 'union3 .005 (test-task1 (union A D) functions) result-union3)
(check-exp-part 'union4 .005 (test-task1 (union (union A B) (union C D)) functions) result-union4)
(check-exp-part 'intersection1 .01 (test-task1 (intersection A B) functions) result-intersection1)
(check-exp-part 'intersection2 .01 (test-task1 (intersection B C) functions) result-intersection2)
(check-exp-part 'intersection3 .01 (test-task1 (intersection C D) functions) result-intersection3)
(check-exp-part 'intersection4 .01 (test-task1 (intersection (intersection A B) (intersection  C D)) functions) result-intersection4)
(check-exp-part 'complements1 .01 (test-task1 (complements A B) functions) result-complements1)
(check-exp-part 'complements2 .01 (test-task1 (complements C D) functions) result-complements2)
(check-exp-part 'complements3 .01 (test-task1 (complements C D) functions) result-complements3)
(check-exp-part 'complements4 .01 (test-task1 (complements (complements A B) (complements C D)) functions) result-complements4)
(check-exp-part 'insert1 .005 (test-task1 (insert B -7) functions) result-insert1)
(check-exp-part 'insert2 .005 (test-task1 (insert A 59525) functions) result-insert2)
(check-exp-part 'insert3 .005 (test-task1 (insert C 988522) functions) result-insert3)
(check-exp-part 'insert4 .005 (test-task1 (insert D -812612) functions) result-insert4)
(check-exp-part 'remove1 .02 (test-task1 (remove binary-search-tree (minimum binary-search-tree)) functions) result-remove1)
(check-exp-part 'remove2 .02 (test-task1 (remove binary-search-tree 9) functions) result-remove2)
(check-exp-part 'remove3 .02 (test-task1 (remove binary-search-tree 3) functions) result-remove3)
(check-exp-part 'remove4 .02 (test-task1 (remove (remove (remove A (successor A 10)) (predecessor A 0)) 416) functions) result-remove4)
(check-exp-part 'complex1 .02 (test-task1 (union A (intersection B C)) functions) result-complex1)
(check-exp-part 'complex2 .02 (test-task1 (insert (intersection (complements B C) (remove (union A B) (predecessor A 51))) 7851) functions) result-complex2)
(check-exp-part 'complex3 .02 (test-task1 (insert (remove (remove (union (intersection (complements B C) (complements B A)) binary-search-tree) 214) 1) 1) functions) result-complex3)
(check-exp-part 'complex4 .02 (test-task1 (union (intersection (complements B A) (union C D)) (complements A D)) functions) result-complex4)
(check-exp-part 'complex5 .02 (test-task1 (intersection (union (complements A B) (complements C D)) (complements (intersection A B) (intersection C D))) functions) result-complex5)
(check-exp-part 'complex6 .02 (test-task1 (remove (insert (union (union (complements A B) (intersection C D)) (intersection (complements C D) (intersection A B))) 22) -8) functions) result-complex6)
(check-exp-part 'complex7 .02 (test-task1 (union (union (intersection A C) (complements A D)) (intersection (complements B C) (intersection B D))) functions) result-complex7)
(check-exp-part 'complex8 .02 (test-task1 (union (union (union A B) (union C D)) (intersection (intersection A B) (intersection C D))) functions) result-complex8)
(check-exp-part 'complex9 .02 (test-task1 (intersection (union (complements A B) (complements B A)) (intersection (union A B) (union C D))) functions) result-complex9)
(check-exp-part 'complex10 .02 (test-task1 (insert (remove (intersection (union (complements B A) (union (complements C D) (intersection A B))) (intersection (complements B (union A C)) (union C D))) 485) 100) functions) result-complex10)
(check-exp-part 'height-balanced1 .04 (check-self-balancing-tree B functions) #t)
(check-exp-part 'height-balanced2 .04 (check-self-balancing-tree A functions) #t)
(check-exp-part 'height-balanced3 .04 (check-self-balancing-tree C functions) #t)
(check-exp-part 'height-balanced4 .04 (check-self-balancing-tree D functions) #t)
(check-exp-part 'height-balanced5 .04 (let [(tree (create-tree '(1 2 3 4 5 6 7 8 9 10) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced6 .04 (let [(tree (create-tree '(20 19 18 17 16 15 10 9 8 7 6 5 4 3 2 1) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced7 .04 (let [(tree (union A (intersection B C)))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced8 .04 (let [(tree (remove (insert (union (complements A D) (intersection B C)) 24) 416))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced9 .04 (let [(tree (union (remove binary-search-tree 9) A))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced10 .04 (let [(tree (intersection (union (remove A (get-value A)) (remove B (get-value B))) (remove C (get-value C))))] (check-self-balancing-tree tree functions)) #t)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - TASK 2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 2 : 20 puncte) ;;check-exp
(check-exp-part 'k-subsets1 0.1 (test-subsets (k-subsets (intersection A B) 8) result-k-subsets1) #t)
(check-exp-part 'k-subsets2 0.1 (let [(subsets (k-subsets binary-search-tree 11))] (and (= (length subsets) 78) (not (equal? (member '(2 3 5 6 8 9 11 12 13 15 21) subsets) #f)))) #t)
(check-exp-part 'k-subsets3 0.1 (test-subsets (k-subsets (create-tree '(1 2 3 4 5) functions) 3) result-k-subsets3) #t)
(check-exp-part 'k-subsets4 0.1 (test-subsets (k-subsets (create-tree '(8 7 6 5) functions) 2) result-k-subsets4) #t)
(check-exp-part 'k-subsets5 0.1 (test-subsets (k-subsets D 20) result-k-subsets5) #t)
(check-exp-part 'zig-zag-subsets1 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 2 3 4 5 6) functions)) result-zig-zag1) #t)
(check-exp-part 'zig-zag-subsets2 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 2 3 4) functions)) result-zig-zag2) #t)
(check-exp-part 'zig-zag-subsets3 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 7 9 10 5) functions)) result-zig-zag3) #t)
(check-exp-part 'zig-zag-subsets4 0.1 (test-subsets (zig-zag-subsets (create-tree '(98 5 1 -85 -457) functions)) result-zig-zag4) #t)
(check-exp-part 'zig-zag-subsets5 0.1 (length (zig-zag-subsets (create-tree '(982 616 542 125 98 85) functions))) 122)
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 0.1 (test-bonus (parser '(1 + (((2 * 3) - 4) * 5))) functions) 11)
(check-exp-part 'bonus2 0.1 (test-bonus (parser '((((5 + 8) * (9 - (8 / 2))) + (8 * 9)) * 10)) functions) 1370)
(check-exp-part 'bonus3 0.1 (test-bonus (parser '((5 * 8) - (7 * (3 + (5 * (10 / 2)))))) functions) -156)
(check-exp-part 'bonus4 0.1 (test-bonus (parser '(((((80 - 78) + 15) * 4 ) / 2) + (7 + (((5 * 3) - 2) * 4)))) functions) 93)
(check-exp-part 'bonus5 0.2 (test-bonus (parser '(((((((((5 + 8) + (9 + 8)) * 3) + (8 - 7)) * 2) + 10) / 2) * 10) - (5 + (7 + (8 * (1 + 2)))))) functions) 924)
(check-exp-part 'bonus6 0.2 (test-bonus (parser '((((((5 + 6) * 7) + 9) * 10) / 2) + (7 * (2 * (4 * (10 - (7 + (1 * (2 - 1))))))))) functions) 542)
(check-exp-part 'bonus7 0.2 (test-bonus (parser '(((5 + (7 - (2 * (3 + (9 - (7 + (8 + (5 * 2)))))))) + (5 * (((2 + 2) * (3 + 7)) + (7 * (9 - (4 + 7)))))) / 2)) functions) 84)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE

(sumar)

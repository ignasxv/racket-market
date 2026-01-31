;;;; ***************************************************************************
;;;; Ignas Kamugisha (iak36)
;;;; Racket/Scheme Programming Exercise
;;;; ***************************************************************************

;;;; ***************************************************************************
;;;; Helper Functions (General)
;;;; ***************************************************************************


#lang racket

;;; atom? returns true if x is not a pair and not null
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;;; ***************************************************************************
;;;; Part 1: Basic Recursion
;;;; ***************************************************************************

;; inorder? takes a list of numbers and returns #t if the numbers are in 
;; non-decreasing order.
(define inorder?
  (lambda (lis)
    (cond
      ;; Base Case: Empty list or single element is always sorted
      [(null? lis) #t]
      [(null? (cdr lis)) #t]
      ;; Recursive Step: Check if first <= second, then check the rest
      [(<= (car lis) (car (cdr lis)))
       (inorder? (cdr lis))]
      [else #f])))

;; dotproduct takes two vectors (lists of numbers) and computes the dot product
;; of the vectors.
(define dotproduct
  (lambda (vec1 vec2)
    (cond
      ;; Base Case: If either list is empty, the sum is 0
      [(or (null? vec1) (null? vec2)) 0]
      ;; Recursive Step: Multiply heads and add to the dotproduct of tails
      [else
       (+ (* (car vec1) (car vec2))
          (dotproduct (cdr vec1) (cdr vec2)))])))

;; squareroot takes a value and an iteration count. It computes the squareroot 
;; using Newton's method recursively.
(define squareroot
  (lambda (val iter)
    (cond
      ;; Base Case: 0 iterations 
      [(zero? iter) val]
      ;; Recursive Step: Calculate previous approximation (old) and apply formula
      [else
       (let ([old (squareroot val (- iter 1))])
         (- old
            (/ (- (* old old) val)
               (* 2 old))))])))

;; removesubsequence takes a subsequence list and a main list. It returns the 
;; main list with the first occurrence of the subsequence removed in order.
(define removesubsequence
  (lambda (sub main)
    (cond
      ;; Base Case 1: If sub is empty, we are done, return the rest of main
      [(null? sub) main]
      ;; Base Case 2: If main is empty but sub is not, just return empty
      [(null? main) '()]
      ;; Match Case: If items match, discard from main and look for next sub item
      [(eq? (car sub) (car main))
       (removesubsequence (cdr sub) (cdr main))]
      ;; No Match: Keep the main item and keep looking for the current sub item
      [else
       (cons (car main) (removesubsequence sub (cdr main)))])))

;;;; ***************************************************************************
;;;; Deep Recursion 
;;;; ***************************************************************************

;; reverse* takes a nested list and reverses the contents of the list and all 
;; nested lists.
(define reverse*
  (lambda (lis)
    (cond
      [(null? lis) '()]
      ;; Sublist Case: Reverse the sublist deeply, then put it at the end
      [(list? (car lis))
       (append (reverse* (cdr lis))
               (list (reverse* (car lis))))]
      ;; Atom Case: Put the atom at the end
      [else
       (append (reverse* (cdr lis))
               (list (car lis)))])))

;; first* returns the first (left most) atom in a nested list.
(define first*
  (lambda (lis)
    (cond
      ;; Base Case: If the list is empty, return empty list (edge case)
      [(null? lis) '()]
      ;; Atom Case: If the first element is an atom (or empty list), return it
      [(atom? (car lis)) (car lis)]
      [(null? (car lis)) '()]
      ;; Sublist Case: Recurse down into the first element
      [else
       (first* (car lis))])))

;; last* returns the last (right most) atom in a nested list.
(define last*
  (lambda (lis)
    (cond
      ;; Base Case: If the list is empty, return empty list
      [(null? lis) '()]
      ;; Recursive Step: If there is a cdr, the last element is in there
      [(not (null? (cdr lis)))
       (last* (cdr lis))]
      ;; Sublist Case: If we are at the last element and it is a list, recurse
      [(list? (car lis))
       (last* (car lis))]
      ;; Atom Case: If we are at the last element and it is an atom, return it
      [else
       (car lis)])))

;;;; ****************************************
;;;; Matrices
;;;; *****************************************

;; Helper for numorder*?: Calculates the "value" of an item.
;; If it is a number, return it. If it is a list, sum its contents deeply.
(define calc-value
  (lambda (x)
    (cond
      [(null? x) 0]
      [(number? x) x]
      [(list? (car x))
       (+ (calc-value (car x)) (calc-value (cdr x)))]
      [else
       (+ (car x) (calc-value (cdr x)))])))

;; numorder*? checks if values of entries and sublists are in non-decreasing order.
(define numorder*?
  (lambda (lis)
    (cond
      ;; Base Case: Empty or single-item lists are ordered
      [(null? lis) #t]
      [(null? (cdr lis))
       ;; Must still check if the single item itself (if a list) is ordered
       (if (list? (car lis))
           (numorder*? (car lis)) #t)]
  
      [else
       (let ([val1 (calc-value (car lis))]
             [val2 (calc-value (car (cdr lis)))])
         (and (<= val1 val2)             ;; Check order of current two values
              (if (list? (car lis))      ;; Check internal order of first item
                  (numorder*? (car lis))
                  #t)
              (numorder*? (cdr lis))))]))) ;; Recurse to check the rest

;; Helper for vectormult: Returns the first column of a matrix (list of cars)
(define get-first-col
  (lambda (mat)
    (cond
      [(null? mat) '()]
      [else (cons (car (car mat)) (get-first-col (cdr mat)))])))

;; Helper for vectormult: Returns the rest of the columns (list of cdrs)
(define remove-first-col
  (lambda (mat)
    (cond
      [(null? mat) '()]
      [else (cons (cdr (car mat)) (remove-first-col (cdr mat)))])))

;; vectormult takes a row vector and matrix and multiplies them.
(define vectormult
  (lambda (vec mat)
    (cond
      ;; Base Case: If the matrix row is empty, 
      [(null? (car mat)) '()]
      ;; Recursive Step: Dotproduct vec with first col, then recurse on rest of columns
      [else
       (cons (dotproduct vec (get-first-col mat))
             (vectormult vec (remove-first-col mat)))])))

;; matrixmultiply takes two matrices and multiplies them.
(define matrixmultiply
  (lambda (mat1 mat2)
    (cond
      [(null? mat1) '()]
      ;; Multiply first row of mat1 by mat2, then recurse
      [else
       (cons (vectormult (car mat1) mat2)
             (matrixmultiply (cdr mat1) mat2))])))
(defun fib? ( deep &optional (init 1) (init2 1) )
  ; 1 1 2 3 5 8 13 21
  (if (= deep 0) 
    init2
    (fib? (decf deep) init2 (+ init init2) )))

;http://rosalind.info/problems/dna/
(defun calc-acgt(dna-seq &optional (aa 0) (cc 0) (gg 0) (tt 0) )
  "calculate the number of nucleotids in DNA sequence. For example: AACGCTTGAGC "
  (if (equal dna-seq "" )
    (format t "~a ~a ~a ~a" aa cc gg tt)
     (let ((nucleus (schar dna-seq 0)))
      (cond
        ((equal nucleus #\A) (setf aa (1+ aa)))
        ((equal nucleus #\T) (setf tt (1+ tt)))
        ((equal nucleus #\C) (setf cc (1+ cc)))
        ((equal nucleus #\G) (setf gg (1+ gg))))
      (calc-acgt (subseq dna-seq 1)  aa cc gg tt))))

;http://rosalind.info/problems/rna/
(defun trans-dna2rna(dna-seq)
  "translate nucleotids in DNA sequence to RNA. Ex:ACGTTATG -> ACGUUAUG  "
  ; (substitute #\U #\T "string)"
  (if (not (equal dna-seq "" ))
    (let ((nucleus (schar dna-seq 0)))
      (if (equal nucleus #\T) 
        (setf nucleus #\U))
      (format t "~a" nucleus)
      (trans-dna2rna (subseq dna-seq 1)))))

;http://rosalind.info/problems/revc/
(defun compl-strand-dna(dna-seq)
  "Complementing a Strand of DNA "
  (defun repl-nucl (nucleus)
    (cond
      ((equal nucleus #\A) (setf nucleus #\T))
      ((equal nucleus #\T) (setf nucleus #\A)) 
      ((equal nucleus #\C) (setf nucleus #\G))
      ((equal nucleus #\G) (setf nucleus #\C))))
  (let ((len (length dna-seq)))
        (loop for i from 0 to (1- len)
              do (format t "~a" (repl-nucl (aref dna-seq (- (1- len) i)))))))

;http://rosalind.info/problems/fib/
(defun rabbits ( new month &optional ( repr 0) (repl-factor 1) )
  "Count the number of rabbits with with initial new and  repl-factor after month and 1 month to become replicable"
  ; 
  (if (= month 1)
    (+ repr new)
    (rabbits (* repl-factor repr) (1- month) (+ repr new) repl-factor)))

;http://rosalind.info/problems/gc/
(defun gc-collect (filename)
  "Culculate the most GC content DNA sequence from FASTA format"
  ;filename have to be in UNIX format
  (let ((gc_count 0)(total 0))
    (defun gc_dna (dna) 
      (loop for nucleus across dna
            do (progn 
                 (if (or (equal nucleus #\C) (equal nucleus #\G)) 
                   (incf gc_count))
                 (incf total))))
    (defun get_gc()
      (if (= total 0) 
        0
        (float (/ gc_count total))))

    (defun reset_gc()
      (setf gc_count 0
            total 0))

    (with-open-file (stream1 filename)
      (let ((label nil)(maxim (list 0 "")))
        (loop for line = (read-line stream1 nil ) while line  
            do (when (eql (char line 0) #\>)
                 (when (not (eql label nil))
                   (format t "Label=~a GC=~a ~%" label (get_gc)) ;;
                   (if (> (get_gc) (first maxim)) 
                     (setf maxim (list (get_gc) label))
                     (reset_gc))
                   (setf label (subseq line 1))) ;TODO to fix "" line
                 (gc_dna line)))
        (format t "Label=~a GC=~a ~%" label (get_gc)) ;;
        (if (> (get_gc) (first maxim)) 
          (setf maxim (list (get_gc) label)))
        (format t "~a~%~a" (car (last maxim)) (first maxim)) ;TODO print label name and GC
      (close stream1)))))

;http://rosalind.info/problems/hamm/
(defun hamming-distance(dna1 dna2)
  ;
  (let ((len (length dna1))(hamming 0))
    (loop for i from 0 to (1- len)
          do (if (not (eql (char dna1 i) (char dna2 i)))
               (incf hamming)))
    (format t "Hamming distance ~a" hamming)))


;http://rosalind.info/problems/iprb/
(defun gen-a(count a) 
  (loop for i from 0 below count collect a))

(defun compare-a (a1 a2)
  ; compare A count after mating a1 and a2
  ; A - dominant ; a - recessio
  (cond
    ((or (equal a1 "AA") (equal a2 "AA")) 4)
    ((and (equal a1 "aa") (equal a2 "aa")) 0)
    ((and (search "A" a1) (search "A" a2)) 3)
    (t 2)))

(defun count-a (population a)
  (let ((example (car population))
        (pop1 (cdr population)))
    (if (eq pop1 nil) 
      a
      (progn (loop for x in pop1
                   do (incf a (compare-a example x)))
             (count-a pop1 a)))))

(defun mendel-first-law(k m n)
  ; count all A probabilities
  (float (/ (count-a (append (gen-a k "AA") (gen-a m "Aa") (gen-a n "aa")) 0)
               (* (let ((sum (+ k m n)))
                    (- (/ (* sum (+ sum 1)) 2) sum)) 4))))
;;;;;;;;;;;;;;;;;;;


#| http://rosalind.info/problems/prot/
(alexandria:alist-hash-table '((UUU . F) (CUU . L)))
https://common-lisp.net/project/alexandria/draft/alexandria.html#Hash-Tables
(gethash 'UUU hash)                            
|#
(ql:quickload "alexandria")
(defparameter prot-code (alexandria:alist-hash-table '(
(UUU . F)      (CUU . L)      (AUU . I)      (GUU . V)
(UUC . F)      (CUC . L)      (AUC . I)      (GUC . V)
(UUA . L)      (CUA . L)      (AUA . I)      (GUA . V)
(UUG . L)      (CUG . L)      (AUG . M)      (GUG . V)
(UCU . S)      (CCU . P)      (ACU . T)      (GCU . A)
(UCC . S)      (CCC . P)      (ACC . T)      (GCC . A)
(UCA . S)      (CCA . P)      (ACA . T)      (GCA . A)
(UCG . S)      (CCG . P)      (ACG . T)      (GCG . A)
(UAU . Y)      (CAU . H)      (AAU . N)      (GAU . D)
(UAC . Y)      (CAC . H)      (AAC . N)      (GAC . D)
(UAA . Stop)   (CAA . Q)      (AAA . K)      (GAA . E)
(UAG . Stop)   (CAG . Q)      (AAG . K)      (GAG . E)
(UGU . C)      (CGU . R)      (AGU . S)      (GGU . G)
(UGC . C)      (CGC . R)      (AGC . S)      (GGC . G)
(UGA . Stop)   (CGA . R)      (AGA . R)      (GGA . G)
(UGG . W)      (CGG . R)      (AGG . R)      (GGG . G))))


(defun calc-prot(rna)
  "Calculate protein sequence from RNA string"
  (loop for i from 0 to (1- (* 3 (truncate (/ (length rna) 3)))) by 3 ; step 3, truncate in case non integer 3 divisioned string 
        do (loop for codon-key in (alexandria:hash-table-keys prot-code) 
                 do (when (equal (string codon-key) (subseq rna i (+ i 3)))
                      (format t "~a" (gethash codon-key prot-code))))))


(defun calc-prot-file(filename)
  (with-open-file (stream1 filename)
    (loop for line = (read-line stream1 nil ) while line  
          do(calc-prot line))))
 
;http://rosalind.info/problems/subs/
(defun find-motif(dna dna_motif)
  "Find Motif in DNA sequence"
  (let ((len_motif (length dna_motif)))
    (loop for i from 0 to (- (length dna) len_motif)
          do(when (string= dna_motif (subseq dna i (+ i len_motif)))
              (format t "~a " (1+ i #|to align with rosalind req|#))))))

;;http://rosalind.info/problems/cons/
(defparameter *dna-array* (make-array '(4 1024) :initial-element 0))
(defparameter *dna-array-size* 0)

(defparameter parser 
  (let ((prev_label nil)(iter 0)) 
    #'(lambda (label line)
        (let ((len1 (length line)))
          (unless (equal prev_label label)
            (setf  iter 0))
          (loop for i from 0 to (1- len1)
                do(let ((pos (+ i iter)))
                    (cond
                      ((equal (char line i) #\A) (setf (aref  *dna-array* 0 pos) (incf (aref *dna-array* 0 pos))))
                      ((equal (char line i) #\C) (setf (aref  *dna-array* 1 pos) (incf (aref *dna-array* 1 pos))))
                      ((equal (char line i) #\G) (setf (aref  *dna-array* 2 pos) (incf (aref *dna-array* 2 pos))))
                      ((equal (char line i) #\T) (setf (aref  *dna-array* 3 pos) (incf (aref *dna-array* 3 pos)))))))
           (if (equal prev_label label)
             (setf iter (+ iter len1) *dna-array-size* iter)
             (setf prev_label label))))))

 

(defun parse-fasta(filename parser)
    (with-open-file (stream1 filename)
        (let ((label nil))
          (loop for line = (read-line stream1 nil ) while line  
                do (if (eql (char line 0) #\>)
                     (setf label (subseq line 1)))
                     (funcall parser label line)))
        (close stream1)))


(parse-fasta "/media/sf_tmp/rosalind_cons.txt"  parser )
(defun print-arr()
  (loop for i from 0 to 3
        do(progn(format t "~%~a: " i)
                (loop for j from 0 to (1- *dna-array-size*)
                       do(format t "~a " (aref  *dna-array* i j))))))

(defun find-cons()
  (let ((maxx 0) (nucl 0))
    (loop for j from 0 to (1- *dna-array-size*)
          do(progn(setf maxx 0)
                   (loop for i from 0 to 3
                        do(when (> (aref  *dna-array* i j ) maxx)
                            (setf maxx (aref  *dna-array* i j ) nucl i)))
                   (format t "~a" (cond
                                    ((equal nucl 0) #\A)
                                    ((equal nucl 1) #\C)
                                    ((equal nucl 2) #\G)
                                    ((equal nucl 3) #\T))
                                    )))))
;;;;;;;;;;;

;http://rosalind.info/problems/fibd/
(defun dead-rabbits ( month life &optional (new 1) ( repr 0) (repl-factor 1) ) 
  "Count the number of rabbits with with initial new and  repl-factor after month and 1 month to become replicable"
  ;(dead-rabbits 1 6  (loop for i from 1 to (1- 3) collect 0)) ; after 6 month; living 3 months
  ;* (dead-rabbits 1 89  (loop for i from 1 to (1- 18) collect 0))
  ;
  ;1769967188277700563
  (let ((dead (car (reverse life))))
;    (format t "~a Repr:~a New:~a Will die:~a ~%" life repr new dead)
    (if (= month 1) 
      (+ repr new)
      (dead-rabbits (1- month)  (cons new (reverse (cdr (reverse life)))) (* repl-factor repr) (- (+ repr new) dead) ))))



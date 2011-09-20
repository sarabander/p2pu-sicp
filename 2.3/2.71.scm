
;; With small modification
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge 
       (adjoin-set (make-code-tree (cadr leaf-set)  ; to place higher 
				   (car  leaf-set)) ; weight leaf left
		   (cddr leaf-set)))))

;; n = 5:

(define alphabet1 '((A 1) (B 2) (C 4) (D 8) (E 16)))

;; Tree for this alphabet: 

;;           {A B C D E} 31
;;         ╱╲
;;        ╱  ╲
;;       ╱    ╲ {A B C D} 15
;;  0 → E 16  ╱╲
;;           ╱  ╲
;;          ╱    ╲ {A B C} 7
;;    10 → D 8   ╱╲
;;              ╱  ╲
;;             ╱    ╲ {A B} 3
;;      110 → C 4   ╱╲
;;                 ╱  ╲
;;                ╱    ╲
;;        1110 → B 2    A 1 ← 1111

;; Generator makes equivalent tree: 

(generate-huffman-tree alphabet1)
;; '((leaf E 16)
;;   ((leaf D 8)
;;    ((leaf C 4) ((leaf B 2) (leaf A 1) (B A) 3) (C B A) 7)
;;    (D C B A)
;;    15)
;;   (E D C B A)
;;   31)


;; n = 10:

(define alphabet2 '((A 1)  (B 2)  (C 4)   (D 8)   (E 16) 
		    (F 32) (G 64) (H 128) (I 256) (J 512)))

;; Tree for this alphabet: 

;;            {A B C D E F G H I J} 1023
;;          ╱╲
;;         ╱  ╲
;;        ╱    ╲ {A B C D E F G H I} 511
;;  0 → J 512  ╱╲
;;            ╱  ╲
;;           ╱    ╲ {A B C D E F G H} 255
;;    10 → I 256  ╱╲
;;               ╱  ╲
;;              ╱    ╲ {A B C D E F G} 127
;;      110 → H 128  ╱╲
;;                  ╱  ╲
;;                 ╱    ╲ {A B C D E F} 63
;;        1110 → G 64   ╱╲
;;                     ╱  ╲
;;                    ╱    ╲ {A B C D E} 31
;;          11110 → F 32   ╱╲
;;                        ╱  ╲
;;                       ╱    ╲ {A B C D} 15
;;            111110 → E 16   ╱╲
;;                           ╱  ╲
;;                          ╱    ╲ {A B C} 7
;;              1111110 → D 8    ╱╲
;;                              ╱  ╲
;;                             ╱    ╲ {A B} 3
;;                11111110 → C 4    ╱╲
;;                                 ╱  ╲
;;                                ╱    ╲
;;                  111111110 → B 2    A 1 ← 111111111

;; Generator makes equivalent tree: 

(generate-huffman-tree alphabet2)
;; '((leaf J 512)
;;   ((leaf I 256)
;;    ((leaf H 128)
;;     ((leaf G 64)
;;      ((leaf F 32)
;;       ((leaf E 16)
;;        ((leaf D 8)
;;         ((leaf C 4) ((leaf B 2) (leaf A 1) (B A) 3) (C B A) 7)
;;         (D C B A)
;;         15)
;;        (E D C B A)
;;        31)
;;       (F E D C B A)
;;       63)
;;      (G F E D C B A)
;;      127)
;;     (H G F E D C B A)
;;     255)
;;    (I H G F E D C B A)
;;    511)
;;   (J I H G F E D C B A)
;;   1023)

;; 1 bit for most frequent symbol, n-1 bits for least frequent symbol.

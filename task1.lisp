;; Вставка в список по номеру
(defun insert (value list index)
  (cond ((= index 0) (push value list))
        ((> index (length list)) nil)
        (t
          (let ((start (subseq list 0 index))
                (end (subseq list index (position (last list) list))))
            (append start (list value) end)))))

(insert 13 (list 4 16 6 19 42 3 51 11) 3)

;; Удаление из списка по позиции
(defun index_removing (index list)
  (cond ((null list) nil)
  ((zerop index) (cdr list))
  (t (cons (car list) (index_removing (1- index) (cdr list))))))
(index_removing 4 (list 4 16 6 19 42 3 51 11))

;; Поиск элемента по значению
(defun searching (value list &optional (position 0) (result nil))
    (if (null list) 
    (if result (reverse result) 0)
    (if (equal value (car list))
        (searching value (cdr list) (+ position 1) (cons position result))
        (searching value (cdr list) (+ position 1) result))))

(searching 42 (list 4 16 6 19 42 3 51 11))

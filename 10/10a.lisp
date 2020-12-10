(defun read-file-as-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun line-as-list (line)
  (read-from-string line))

(defun parse-input (filename)
  (mapcar #'line-as-list (read-file-as-lines filename)))

(defun build-joltages (filename)
  (let* ((adapter-joltages (sort (parse-input filename) #'<))
         (outlet-joltage 0)
         (max-joltage (car (last adapter-joltages)))
         (device-joltage (+ max-joltage 3)))
    (cons outlet-joltage (append adapter-joltages (cons device-joltage Nil)))))

(defun get-joltage-pairs (list)
  (mapcar #'cons list (cons 0 list)))

(defun get-pair-difference (pair)
  (- (car pair) (cdr pair)))

(defun solve-a (filename)
  (let* ((joltages (build-joltages filename))
         (joltage-pairs (get-joltage-pairs joltages))
         (pair-differences (mapcar #'get-pair-difference joltage-pairs))
         (joltage-1-differences (remove-if-not (lambda (x) (eql x 1)) pair-differences))
         (joltage-3-differences (remove-if-not (lambda (x) (eql x 3)) pair-differences)))
    (* (list-length joltage-1-differences) (list-length joltage-3-differences))))

(print (solve-a "input.txt"))

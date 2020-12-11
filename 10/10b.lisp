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

(defun count-paths (actual-joltages check-joltages prev-joltage-3 prev-joltage-2 prev-joltage-1)
  (let ((total-paths (+ prev-joltage-3 prev-joltage-2 prev-joltage-1)))
    (if check-joltages
      (if (member (car check-joltages) actual-joltages)
        (count-paths actual-joltages (cdr check-joltages) prev-joltage-2 prev-joltage-1 total-paths)
        (count-paths actual-joltages (cdr check-joltages) prev-joltage-2 prev-joltage-1 0))
      total-paths)))

(defun solve-b (filename)
  (let* ((joltages (build-joltages filename))
         (max-joltage (car (last joltages)))
         (check-joltages (loop for n below (+ max-joltage 1) collect n)))
    (count-paths joltages check-joltages 1 0 0)))

(print (solve-b "input.txt"))

(require 'ert)
(require 'pwstore)

(defmacro pwstore-test-with-alist (alist &rest body)
  ""
  (declare (indent 1))
  `(unwind-protect
       (progn
         (with-temp-file pwstore-file
           (let (print-length print-level)
             (prin1 ,alist (current-buffer))))
         ,@body)
     (when (file-exists-p pwstore-file)
       (delete-file pwstore-file))))

(ert-deftest pwstore-test-names ()
  ""
  (let ((pwstore-file (expand-file-name (make-temp-name "pwstore")
                                        temporary-file-directory)))
    (should-not (pwstore-names))
    (pwstore-test-with-alist nil
      (should-not (pwstore-names)))
    (pwstore-test-with-alist '(("name1" :key1 "value1" :key2 "value2")
                               ("name2" :key1 "value1" :key2 "value2"))
      (should (equal (pwstore-names) '("name1" "name2"))))))

(ert-deftest pwstore-test-secret-p ()
  ""
  (let ((pwstore-password-key :key1)
        (pwstore-secret-keys '(:key2 :key3)))
    (should (cl-every 'pwstore-secret-p '(:key1 :key2 :key3)))
    (should-not (pwstore-secret-p :key4))))

(ert-deftest pwstore-test-empty-p ()
  ""
  (let ((pwstore-file (expand-file-name (make-temp-name "pwstore")
                                        temporary-file-directory)))
    (pwstore-test-with-alist nil
      (pwstore-with-store
        (should (pwstore-empty-p store))))
    (pwstore-test-with-alist '(("name1" :key1 "value1" :key2 "value2")
                               ("name2" :key1 "value1" :key2 "value2"))
      (pwstore-with-store
        (should-not (pwstore-empty-p store))))))

;; does not work in batch mode
;; (ert-deftest pwstore-test-read-name ()
;;   ""
;;   (let ((unread-command-events (listify-key-sequence (kbd "name1 RET"))))
;;     (should (equal (pwstore-read-name) "name1"))))

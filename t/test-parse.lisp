
(in-package :test.discworld)

(deftest test/odm121 ()
  (let ((root (parse-odm (find-test-file "odm121.xml"))))
    (check (string= "1.2"
		    (property root :|ODMVersion|)))
    (check (string= "Transactional"
		    (property root :|FileType|)))
    (check (string= "000-00-0000"
		    (property root :|FileOID|)))))

(deftest test/odm130 ()
  (let ((root (parse-odm (find-test-file "odm130.xml"))))
    (check (string= "1.3"
		    (property root :|ODMVersion|)))
    (check (string= "Transactional"
		    (property root :|FileType|)))
    (check (string= "000-00-0000"
		    (property root :|FileOID|)))))

(deftest test/parse ()
  (test/odm121)
  (test/odm130)) ;; fails because s-xml tries to read as utf-8

(test/parse)

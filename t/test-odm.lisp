
(in-package :test.discworld)

(deftest test/odm ()
  (test/odm121)
  (test/odm130)
  (test/oid)
  (test/name))

(deftest test/odm121 ()
  (let ((root (odm:parse-odm (find-test-file "odm121.xml"))))
    (check (string= "1.2"
		    (odm:property root :|ODMVersion|)))
    (check (string= "Transactional"
		    (odm:property root :|FileType|)))
    (check (string= "000-00-0000"
		    (odm:property root :|FileOID|)))))


(deftest test/odm130 ()
  (let ((root (odm:parse-odm (find-test-file "odm130.xml"))))
    (check (string= "1.3"
		    (odm:property root :|ODMVersion|)))
    (check (string= "Transactional"
		    (odm:property root :|FileType|)))
    (check (string= "000-00-0000"
		    (odm:property root :|FileOID|)))))


(deftest test/oid ()
  (let* ((root (odm:parse-odm (find-test-file "odm121.xml")))
	 (study (odm:find-one root :test (odm:of-elem-type 'study)))
	 (group (odm:find-one root :test (odm:of-elem-type 'itemgroupdef)))
	 (ref (first (odm:items group))))
    (check (string= "000-00-0000" (odm:oid root)))
    (check (string= "StudyOID" (odm:oid study)))
    (check (string= "PARTIAL" (odm:oid group)))
    (check (string= "ID.PD" (odm:oid ref)))))


(deftest test/name ()
  (let* ((root (odm:parse-odm (find-test-file "odm121.xml")))
	 (form (odm:find-one root :test (odm:of-elem-type 'formdef)))
	 (group (odm:find-one root :test (odm:of-elem-type 'itemgroupdef))))
    (check (string= "Form Definition" (odm:name form)))
    (check (string= "ItemData Extension" (odm:name group)))))


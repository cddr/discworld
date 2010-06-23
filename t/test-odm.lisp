
(in-package :dw.test)

(deftest test/odm ()
  (test/odm121)
  (test/odm130)
  (test/oid)
  (test/name)
  (test/path))


(deftest test/odm121 ()
  (let ((root (odm-parse (find-test-file "odm121.xml"))))
    (check (string= "1.2"
		    (property root :|ODMVersion|)))
    (check (string= "Transactional"
		    (property root :|FileType|)))
    (check (string= "000-00-0000"
		    (property root :|FileOID|)))))


(deftest test/odm130 ()
  (let ((root (odm-parse (find-test-file "odm130.xml"))))
    (check (string= "1.3"
		    (property root :|ODMVersion|)))
    (check (string= "Transactional"
		    (property root :|FileType|)))
    (check (string= "000-00-0000"
		    (property root :|FileOID|)))))


(deftest test/oid ()
  (let* ((root (odm-parse (find-test-file "odm121.xml")))
	 (study (find-one root :test (of-elem-type 'study)))
	 (group (find-one root :test (of-elem-type 'itemgroupdef)))
	 (ref (first (items group))))
    (check (string= "000-00-0000" (oid root)))
    (check (string= "StudyOID" (oid study)))
    (check (string= "PARTIAL" (oid group)))
    (check (string= "ID.PD" (oid ref)))))


(deftest test/name ()
  (let* ((root (odm-parse (find-test-file "odm121.xml")))
	 (form (find-one root :test (of-elem-type 'formdef)))
	 (group (find-one root :test (of-elem-type 'itemgroupdef))))
    (check (string= "Form Definition" (name form)))
    (check (string= "ItemData Extension" (name group)))))

(deftest test/path ()
  (let* ((root (odm-parse (find-test-file "odm121.xml")))
	 (form (find-one root :test (of-elem-type 'formdef)))
	 (group (find-one root :test (of-elem-type 'itemgroupdef))))
    (check (string=
	    "document('000-00-0000')/ODM/Study[@OID='StudyOID']/MetaDataVersion[@OID='v1.2.1']/FormDef[@OID='FormOID']"
	    (odm-path form)))
    (check (string= 
	    "document('000-00-0000')/ODM/Study[@OID='StudyOID']/MetaDataVersion[@OID='v1.2.1']/ItemGroupDef[@OID='PARTIAL']"
	    (odm-path group))
    (check (string= 
	    "document('000-00-0000')/ODM"
	    (odm-path root))))))




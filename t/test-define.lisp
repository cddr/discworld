
(in-package :test.discworld)

;; test/domains
;; test/variables
;; test/schema

(deftest test/define ()
  (test/domains)
  (test/variables)
  (test/schema))

(deftest test/domains ()
  (let* ((adae (find-domain "define-cdiscpilot01.xml" "ADAE"))
	 (suppdm (find-domain "define-cdiscpilot01.xml" "SUPPDM")))
    (check (string= "ADAE" (name adae)))
    (check (string= "SUPPDM" (name suppdm)))))

(deftest test/variables ()
  (let* ((dm (find-domain "define-cdiscpilot01.xml" "DM"))
	 (studyid (first (variables dm))))
    (check (string= "STUDYID" (name studyid)))
    (check (string= "Study Identifier" (label studyid)))
    (check (string= "text" (data-type studyid)))
    (check (string= "CRF Page 7" (origin studyid)))))

(deftest test/schema ()
  (let* ((dm (find-domain "define-cdiscpilot01.xml" "DM"))
	 (schema (schema dm)))
    (check (string= schema "
create table DM (
  STUDYID,
  USUBJID,
  DOMAIN,
  SUBJID,
  RFSTDTC,
  RFENDTC,
  SITEID,
  AGE,
  AGEU,
  SEX,
  RACE,
  ARMCD,
  ARM,
  COUNTRY,
  DMDTC,
  DMDY
);"))))


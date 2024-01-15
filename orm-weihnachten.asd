;;;; orm-weihnachten.asd

(when (find-package :slynk) (pushnew :slynk *features*))
(when (find-package :swank) (pushnew :swank *features*))

(asdf:defsystem #:orm-weihnachten
  :description "Code für Weihnachten 2023"
  :author "Ugo + Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :depends-on (:clog-widgets)
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "main")))

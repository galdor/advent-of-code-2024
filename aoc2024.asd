(defsystem "aoc2024"
  :description "Solutions for the Advent of Code 2024 event."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :license "ISC"
  :pathname "src"
  :depends-on
  ("tungsten-core"
   "tungsten-http")
  :serial t
  :components
  ((:file "utils")
   (:file "day-01")
   (:file "day-02")
   (:file "day-03")
   (:file "day-04")
   (:file "day-05")
   (:file "day-06")
   (:file "day-07")
   (:file "day-08")
   (:file "day-09")
   (:file "day-10")
   (:file "day-11")))

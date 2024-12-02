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
   (:file "day-02")))

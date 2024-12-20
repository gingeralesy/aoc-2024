#|
This file is a part of aoc-2024
(c) 2024 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem aoc-2024
  :version "0.0.0"
  :license "zlib"
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "Advent of Code 2024 - https://adventofcode.com/2024"
  :serial T
  :components ((:file "package")
               (:file "input")
               (:file "util")
               (:file "queue")
               (:file "heap")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13")
               (:file "day14")
               (:file "day15")
               (:file "day16")
               (:file "day17")
               (:file "day18")
               (:file "day19")
               (:file "day20"))
  :depends-on (:asdf
               :alexandria
               :cl-ppcre
               :local-time
               :bordeaux-threads
               :skippy))

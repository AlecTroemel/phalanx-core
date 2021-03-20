(declare-project
 :name "phalanx"
 :description "the root logic for a game called phalanx"
 :dependencies ["https://github.com/joy-framework/tester"]
 :source ["src/phalanx.janet"]
 :author "Alec Troemel"
 :url "https://github.com/AlecTroemel/phalanx-core"
 :repo "git+https://github.com/joy-framework/joy")

(declare-source
 :source @["src/phalanx.janet"])

(declare-project
 :dependencies ["https://github.com/joy-framework/tester"])

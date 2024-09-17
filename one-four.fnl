#!/usr/bin/env fennel
(local inspect (require "inspect"))

(global stack [])
(global words {})
(local one4 {})

(macro push [item]
  `(table.insert _G.stack ,item))

(macro pop []
  `(table.remove _G.stack))

(macro peek []
  `(. _G.stack (length _G.stack)))

;; Helpers to wrap higher level functions into forth level
;; assume a single return value
(fn func-unary [f]
  (let [arg (pop)]
    (push (f arg))))

(fn func-binary [f]
  (let [arg1 (pop)
        arg2 (pop)]
    (push (f arg2 arg1))))

(fn one4.handle-word [word]
  (case (type (. words word))
    "table" (each [i w (ipairs (. words word))]
              (one4.eval w))
    _ (push word)))

(fn one4.eval [w]
  (if (and (= one4.mode "compile") (not (= w ";")))
      (do 
        ; in compile mode - simply push words onto the store
        ; the top of the stack is the word name already
        (table.insert (. words one4.curr) w)
        ; now do some processing for if/else/then using the stack itself
        )
      (case w
        (where num (tonumber num)) (push (tonumber num))
        "exit" (os.exit)
        ".s" (inspect _G.stack)
        "." (pop)
        "+" (func-binary #(+ $1 $2))
        "-" (func-binary #(- $1 $2))
        "*" (func-binary #(* $1 $2))
        "abs" (func-unary math.abs)
        "=" (func-binary #(= $1 $2))
        "dup" (push (peek))
        "var" (tset words (pop) nil)
        "!" (func-binary #(tset words $2 $1))
        "?" (func-unary #(. words $1))
        ":" (do (tset one4 :mode "compile") (tset one4 :curr (pop)) (tset words one4.curr []))
        ";" (do (tset one4 :mode "eval") (.. one4.curr " defined"))
        (where word (not (= nil (. words word)))) (one4.handle-word word) ; word is in store
        _ (push w)))) ; unknown word

(fn repl []
  (while true
    (io.write "> ")
    (let [a (io.read "*l")]
      (each [w (string.gmatch a "%S+")]
        (local res (one4.eval w))
        (if res
            (print res))))))

(fn eval-file [filename]
  (let [file (io.open filename "r")]
    (each [l (io.lines filename)]
      (each [w (string.gmatch l "%S+")]
        (one4.eval w)))
    (file:close)))

(let [file (. arg 1)]
  (if (not (= nil file))
    (eval-file file)))
(repl)

#!/usr/bin/env fennel
(local inspect (require "inspect"))

(global stack [])
(global words {})
(local one4 {:curr "" :offset 1})

(macro push [item] `(table.insert _G.stack ,item))
(macro pop [] `(table.remove _G.stack))
(macro peek [] `(. _G.stack (length _G.stack)))

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
    ;; compiled word
    "table" (do
              (local def (. words word))
              (var ptr 1)
              (while (<= ptr (length def))
                (case (. def ptr) ;; special pos aware eval
                  "cjmp" (let [off (pop)
                               cond (pop)]
                           (if (not cond)
                               (set ptr (+ off ptr))
                               (set ptr (+ 1 ptr))))
                  "jmp" (set ptr (+ (pop) ptr))
                  _ (do ;; fallback to normal eval process
                      (one4.eval (. def ptr))
                      (set ptr (+ 1 ptr))))))
    _ (push word)))

(fn one4.eval [w]
  (if (and (= one4.mode "compile") (not (= w ";")))
      (do 
        ; do some processing for if/else/then using the stack itself
        ; at compile time
        (case w
          "if" (do
                 (one4.eval 0) ;; add dummy jump target to def
                 (push one4.offset) ;; save the index of this word in this def onto stack
                 (one4.eval "cjmp")) ;; add the cjmp instruction
          "else" (do
                   (local target (pop)) ;; get the saved target, loc of if
                   (one4.eval 0) ;; add dummy jump offset
                   (push one4.offset) ;; save curr off to stack
                   (one4.eval "jmp") ;; add the jmp word
                   ;; adjust the jump target of if to the current offset
                   (tset (. words one4.curr) target (- one4.offset target)))
          "fi" (do
                 (let [target (pop) ;; get the corresponding if/else condition loc
                       off (- one4.offset target)] ;; calculate diff between fi and if
                   (tset (. words one4.curr) target off))) ;; save this value into the def
          _ (do
              (table.insert (. words one4.curr) w) ; add word as it is
              (set one4.offset (+ 1 one4.offset)) ; update offset
              )))
      (case w
        (where num (tonumber num)) (push (tonumber num))
        "exit" (os.exit)
        ".s" (inspect _G.stack)
        "." (pop)
        "+" (func-binary #(+ $1 $2))
        "-" (func-binary #(- $1 $2))
        "*" (func-binary #(* $1 $2))
        "%" (func-binary #(% $1 $2))
        "abs" (func-unary math.abs)
        "=" (func-binary #(= $1 $2))
        "dup" (push (peek))
        "swap" (let [a (pop) b (pop)] (push a) (push b))
        "var" (tset words (pop) nil)
        "!" (func-binary #(tset words $2 $1))
        "?" (func-unary #(. words $1))
        ":" (do (set one4.mode "compile") (set one4.curr (pop))
                (set one4.offset 0) (tset words one4.curr []))
        ";" (do (tset one4 :mode "eval") (.. one4.curr " defined")
                (print (inspect (. words one4.curr))))
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

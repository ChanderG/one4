(local inspect (require "inspect"))

(global stack [])
(global words {:square ["dup" "*"]})
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
    (where word (not (= nil (. words word)))) (one4.handle-word word) ; word is in store
    _ (push w))) ; unknown word

(fn repl []
  (while true
    (io.write "> ")
    (let [a (io.read "*l")]
      (each [w (string.gmatch a "%S+")]
        (local res (one4.eval w))
        (if res
            (print res))))))

(repl)

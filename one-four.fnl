(global stack [])
(global words {})

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

;; math functions
(fn add [x y]
  (+ x y))

(fn sub [x y]
  (- x y))

(fn handle-word [])

(fn eval [w]
  (case w
    (where num (tonumber num)) (push (tonumber num))
    "exit" (os.exit)
    ".s" (.. "[" (table.concat _G.stack " ") "]")
    "." (pop)
    "+" (func-binary add)
    "-" (func-binary sub)
    "abs" (func-unary math.abs)
    "dup" (push (peek))
    (where word (not (= nil (. _G.words word)))) (handle-word)
    _ w))

(fn repl []
  (while true
    (io.write "> ")
    (let [a (io.read "*l")]
      (each [w (string.gmatch a "%S+")]
        (local res (eval w))
        (if res
            (print res))))))

(repl)

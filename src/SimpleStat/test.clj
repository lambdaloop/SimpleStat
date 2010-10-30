(defn fun1 [msg]
  (println msg)
  (recur msg))


(.start (Thread. #(fun1 "hello")))
(.start (Thread. #(fun1 "world")))


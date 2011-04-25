;Version: 0.1.0
;Copyright: Eduardo Emilio Julián Pereyra, 2010
;Email: eduardoejp@gmail.com
;License: EPL 1.0 -> http://www.eclipse.org/legal/epl-v10.html

(ns thor
  #^{:doc "Implementation of stack-oriented programming for Clojure."
     :author "Eduardo Emilio Julián Pereyra"}
  (:use (clojure.contrib def))
  (:refer-clojure :exclude [drop])
  (:require (clojure.contrib [error-kit :as err])))

(let [push (fn [atom stack] (conj stack atom))
      word? (fn [word] (-> word type (= ::Word)))]
  (defn _thor [stack & elems]
    (loop [*stack* stack, [atom & _] elems]
      (if-not (and (nil? atom) (nil? _))
        (cond
          (#{< <= > >= = not= + - * /} atom) (let [[y x & _stack] *stack*] (recur (push (atom x y) _stack) _))
          (word? atom) (recur (atom *stack*) _)
          :else (recur (push atom *stack*) _))
        *stack*))))

(defn thor "Executes the given code in a new stack and returns the resulting stack."
  [& elems] (apply _thor '() elems))

(defn as-word
  "Adapts a regular fn so it can be used as a Thor Word."
  [f n]
  (with-meta
    (fn [stack]
      (let [r (apply f (take n stack)), r (if r (list r) '())]
        (concat r (nthnext stack n))))
    {:type ::Word}))

(defmacro word-let
  "Given a bindings vector with fn-argsnum pairs, create adapted words from those functions for easy usage inside the body."
  [binds & body]
  `(let ~(vec (apply concat (for [[f n] (partition 2 binds)] `[~f (adapt ~f ~n)]))) ~@body))

(defmacro defword
  "Defines a Word by providing a sequence of words and other primitives to execute."
  [sym doc-string & words]
  `(def ~(with-meta sym {:doc (str doc-string)})
     (with-meta (fn ~sym [~'*stack*] (_thor ~'*stack* ~@words)) {:type ::Word})))

(defmacro defword+
"Defines a word that can direclty manipulate the stack and is written using regular Clojure code, instead of the postfix stack-based code.
The stack can be accesed through the *stack* binding and is a regular Clojure sequence. This function must return a new list/stack that will be used as the stack for the next words."
  [sym doc-string & body]
  `(def ~(with-meta sym {:doc (str doc-string)})
     (with-meta (fn ~sym [~'*stack*] ~@body) {:type ::Word})))

; From here on forward, I'll be defining the Thor "Standard Library" :P
(defword+ dup "[x -> x x]" (conj *stack* (first *stack*)))
(defword+ dup-n "Duplicate the upper N ammount of items in the Stack." (concat (take (first *stack*) (rest *stack*)) (rest *stack*)))
(defword dup-2 "[x y -> x y x y]" 2 dup-n)
(defword+ drop "[x y -> x]" (clojure.core/drop 1 *stack*))
(defword+ drop-n "Drops the top N items." (clojure.core/drop (first *stack*) (rest *stack*)))
(defword drop-2 "[x y -> ]" 2 drop-n)
(defword+ nip "[x y -> y]" (conj (nnext *stack*) (first *stack*)))
(defword+ nip-n "Nips the upper N items in the Stack after the top." (conj (nthnext (rest *stack*) (inc (first *stack*))) (second *stack*)))
(defword+ over "[x y -> x y x]" (conj *stack* (second *stack*)))
(defword+ over-n "Same as over, but with variable length." (concat (take (first *stack*) (nnext *stack*)) (rest *stack*)))
(defword+ swap "[x y -> y x]" (-> (clojure.core/drop 2 *stack*) (conj (first *stack*) (second *stack*))))
(defword+ pick "[x y z -> x y z x]" (conj *stack* (last *stack*)))
(defword+ rot "[x y z -> y z x]" (conj (butlast *stack*) (last *stack*)))
(defword+ -rot "[x y z -> z x y]" (concat (rest *stack*) (list (first *stack*))))
(defword+ run-quote "Runs a quote pushed into the Stack." (apply _thor (rest *stack*) (first *stack*)))
(defword+ !if "[bool [then] [else] -> Whatever is the result of the corresponding quote]"
  (let [[else then bool & _] *stack*, _ (if (nil? _) '() _)]
    (cond
      (true? bool) (_thor _ then run-quote) ; The case for regular booleans and boolean results from Clojure forms.
      (false? bool) (_thor _ else run-quote)
      (vector? bool) (_thor (_thor _ bool run-quote) then else !if) ; The case for embedded quotes.
      :else (_thor (_thor _ bool) then else !if)
      )))
(defword neg "[bool -> (not bool)]" [false] [true] !if)
(defword !when "[bool [then] -> Runs [then] only if bool]" [] !if)
(defword !unless "[bool [then] -> Runs [then] only if not bool]" [] swap !if)
(defword join-quotes "[[a] [b] -> [a b]]" (as-word #(vec (concat %2 %1)) 2))
(defword !while "[bool [while] -> Runs [while] as long as bool is true. Bool can also be a word or a quote.]"
  dup-2 -rot -rot
  [rot rot !while] join-quotes
  [rot rot drop-2] !if)
(defword !until "[bool [until] -> Runs [until] as long as bool is false. Bool can also be a word or a quote.]"
  swap [neg] join-quotes swap !while)

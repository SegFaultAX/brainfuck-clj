(ns brainfuck.core
  (:require [clojure.string :as str]))

;; Helper functions

(defn clean-src
  [src]
  (str/replace src #"[^\[\]<>,.+-]" ""))

;; Jump table

(defn make-table
  [jmps]
  (loop [[jmp & jmps] jmps [t & b :as stk] () acc {}]
    (let [[ji jc] jmp [ti tc] t]
      (cond
        (and (nil? jmp) (empty? stk)) acc
        (= \[ jc) (recur jmps (conj stk jmp) acc)
        (and (= \] jc) (= \[ tc)) (recur jmps b (assoc acc ji ti ti ji))
        :else (throw (IllegalArgumentException. "Invalid program!"))))))

(defn jump-table
  [src]
  (let [cs (map vector (range) src)
        jmps (filter (comp #{\[ \]} second) cs)]
    (make-table jmps)))

;; Instruction pointer

(defn cur-ins
  [prg]
  (nth (:src prg) (:ins-ptr prg)))

(defn inc-ins
  [prg]
  (update-in prg [:ins-ptr] inc))

;; Data/memory value

(defn inc-mem
  [prg]
  (update-in prg [:mem (:mem-ptr prg)] inc))

(defn dec-mem
  [prg]
  (update-in prg [:mem (:mem-ptr prg)] dec))

(defn get-mem
  [prg]
  (get-in prg [:mem (:mem-ptr prg)]))

(defn set-mem
  [prg v]
  (assoc-in prg [:mem (:mem-ptr prg)] v))

;; Data/memory pointer

(defn inc-ptr
  [prg]
  (let [mem-ptr (inc (:mem-ptr prg))
        mem (:mem prg)
        mem (if (>= mem-ptr (count mem)) (conj mem 0) mem)]
    (assoc prg :mem mem :mem-ptr mem-ptr)))

(defn dec-ptr
  [prg]
  (let [mem-ptr (dec (:mem-ptr prg))]
    (if (>= mem-ptr 0)
      (assoc prg :mem-ptr mem-ptr)
      (throw (IllegalArgumentException. "Invalid program!")))))

;; Loops

(defn start-loop
  [prg]
  (let [mem (get-mem prg)
        jmp-to ((:jmp prg) (:ins-ptr prg))]
    (if (zero? mem)
      (assoc prg :ins-ptr jmp-to)
      prg)))

(defn end-loop
  [prg]
  (let [mem (get-mem prg)
        jmp-to ((:jmp prg) (:ins-ptr prg))]
    (if (not (zero? mem))
      (assoc prg :ins-ptr jmp-to)
      prg)))

;; Input/Output

(defn run-input
  [prg]
  (let [[c & in] (:input prg)
        v (int (or c 0))]
    (-> prg (set-mem v) (assoc :input in))))

(defn run-output
  [prg]
  (update-in prg [:output] conj (char (get-mem prg))))

;; Evaluator

(def instructions
  {\+ inc-mem
   \- dec-mem
   \> inc-ptr
   \< dec-ptr
   \. run-output
   \, run-input
   \[ start-loop
   \] end-loop})

(defn evaluate*
  [{:keys [src-len ins-ptr] :as prg}]
  (if (< ins-ptr src-len)
    (let [ins (instructions (cur-ins prg))]
      (-> prg ins inc-ins))
    (assoc prg :done true)))

(defn evaluate
  [prg]
  (first (drop-while #(false? (:done %)) (iterate evaluate* prg))))

(defn init-program
  [src input]
  (let [src (clean-src src)]
    {:src src
     :src-len (count src)
     :ins-ptr 0
     :mem [0]
     :mem-ptr 0
     :jmp (jump-table src)
     :input (seq input)
     :output []
     :done false}))

(comment
  (def sample
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]
     >>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  (init-program sample "")
  (jump-table src)
  (evaluate (init-program sample ""))
)

(ns quince.core)

(def INF 1000000000)
(def ROW_SIZE 4)
(def PUZZLE (* ROW_SIZE ROW_SIZE))
(def X 15)

(def p (int-array PUZZLE))
(def lim (atom 0) )
(def nlim (atom 0))
(def dr (int-array [0 -1 0 1]));;E, N, W, S
(def dc (int-array [1 0 -1 0]));;R, U, L, D
(def pred (new java.util.TreeMap ))
(def vis  (new java.util.TreeMap ))
(def res (char-array "RULD"));;R, U, L, D


;; suma de todas las distancias
;; las distancias manhattan
(defn h1[]
  (let [ans (atom 0)]
  (dotimes [k PUZZLE]
    (let [i (quot (aget p k) 4)
          j (mod  (aget p k) 4)]
      (if (not= (aget p k) X)
        (do
          (reset! ans
            (+ @ans (Math/abs (- (quot k 4) i)) (Math/abs (- (mod k 4) j)) ))
          ))))
    @ans))

(defn h2 [i1 j1 i2 j2]
  (let [tgt_i (quot (aget p (+ (* i2 4) j2)) 4)
        tgt_j (mod  (aget p (+ (* i2 4) j2)) 4)]
    ;;(println (aget p (+ (* i2 4) j2)) tgt_i tgt_j)
    (- (+ (Math/abs (- i1 tgt_i)) (Math/abs (- j1 tgt_j)))
       (+ (Math/abs (- i2 tgt_i)) (Math/abs (- j2 tgt_j))) ) ))

(defn goal []
  (let [ans (atom true)]
    (dotimes [i PUZZLE]
      (if (and (not= (aget p i) X) (not= (aget p i) i) )
        (do
          (reset! ans false))))
    @ans))

(defn valid [r c]
  (and (<= 0 r) (< r 4) (<= 0 c) (< c 4)))

(defn output[d]
  (if (not= d 0)
    (do
      (output (- d 1))
      (print (aget res (.get pred d))))))


(defn imprime [g d]
  (println "\nCantidad de movimientos: "g)

  (println "Movimiento: " (aget res d) )

  (dotimes [i ROW_SIZE]
    (dotimes [j ROW_SIZE]
      (printf "%3d" (mod (+ 1 (aget p (+ (* i 4) j))) 16))
      (print  ""))
    (println)))

(defn swapea [i j new_i new_j]
  (let [temp (aget p (+ (* i 4) j))]
    (aset-int p (+ (* i 4) j) (aget p (+ (* new_i 4) new_j)))
    (aset-int p (+ (* new_i 4) new_j) temp )))

(defn DFS [g h]
  (let [state (atom (long 0))
        answered (atom false)
        ans (atom false)]


    (if (> (+ g h) @lim)
      (do
        (reset! ans false)
        (reset! answered true)
        (reset! nlim (min @nlim (+ g h)))))


    (if (and (not @answered) (goal))
      (do
        (reset! ans true)
        (reset! answered true)))

    (dotimes [i PUZZLE] ;; mascara de bits 16 nums a uno de 64 bits
      (reset! state (bit-shift-left @state 4))
      (reset! state (+ @state (aget p i))))

    ;;(println @state)

    (if (and (not @answered) (.containsKey vis @state) (<= (.get vis @state) g))
      (do
        (reset! ans false)
        (reset! answered true)))



    (if (not @answered)
      (do
        (.put vis @state g)
        (let [i (atom 0)
              j (atom 0)
              new_i (atom 0)
              new_j (atom 0)]
          (dotimes [k PUZZLE]
            (if (= (aget p k) X)
              (do
                (reset! j (mod k 4))
                (reset! i (quot k 4)))))

          (dotimes [d 4]
            (reset! new_i (+ @i (aget dr d)))
            (reset! new_j (+ @j (aget dc d)))
            ;;(println @i @j @new_i @new_j)
            (if (and (not @answered) (valid @new_i @new_j) )
              (do
                (let [dh (h2 @i @j @new_i @new_j)]
                  ;;(println dh)
                  (swapea @i @j @new_i @new_j)
                  (.put pred (+ g 1) d)
                  ;;(imprime (+ g 1) d)
                  (if (DFS (+ g 1) (+ h dh))
                    (do
                      (reset! ans true)
                      (reset! answered true)))
                  (if (not @answered)
                    (do
                      (swapea @i @j @new_i @new_j))))))))))



    @ans))

(defn IDA_Star[]
  (reset! lim (h1))
  (let [flag (atom true)
        ans  (atom @lim)]
    (while @flag
      (do
        (reset! nlim INF);; siguiente limite
        (.clear pred)
        (.clear vis)
        (if (and (DFS 0 (h1)) @flag)
          (do
            (reset! flag false)
            (reset! ans @lim)))
        ;;(println " lims "@lim @nlim @flag)
        (if (and (= @nlim INF) @flag)
          (do
            (reset! flag false)
            (reset! ans -1)))
        (reset! lim @nlim) ;; nlim > lim
        (if (and (> @lim 45) @flag);; se puede incrementar
          (do
            (reset! flag false)
            (reset! ans -1)))))
    ;;(println "la chucha")
    @ans))

(defn main [orivec]
  (let [k (atom 0)
        blank(atom 0)
        suma (atom 0)]
  (doseq [ item orivec]
    (aset-int p @k item)
    (if (= item  15)
      (reset! blank @k))
    (swap! k inc))
  (dotimes [i PUZZLE]
    (dotimes [j i]
      ;;(println "<- " i j " ->" )
      (if (and (not= X (aget p i)) (not= X (aget p j)) (> (aget p j) (aget p i)))
        (reset! suma (inc @suma)))))
  (reset! suma (+ @suma (quot @blank ROW_SIZE)))
  (if (odd? @suma)
    ;; if
    (do
      (let [ans (IDA_Star)]
      ;;(println ans)
      (if (not= ans -1)
        (do
          (println "\nSecuencia Final: ")
          (println ans)
          (output ans)
          (println ""))
        ;;else
        (do
          (println "Esto no tiene solución en 45 pasos")))))
    ;; esle
    (do
      (println "Esto no tiene solución")))
  0 ;; return
  ))

;;2 3 4 0
;;1 5 7 8
;;9 6 10 12
;;13 14 11 15

(main [1 2 3 15 0 4 6 7 8 5 9 11 12 13 10 14])

;;13 1 2 4
;;5 0 3 7
;;9 6 10 12
;;15 8 11 14

(main [12 0 1 3 4 15 2 6 8 5 9 11 14 7 10 13])

;;     4 0 6 2 8 1 10 3 12 5 14 7 15 9 13 11

(main [4 0 6 2 8 1 10 3 12 5 14 7 15 9 13 11])

;;(main [5 1 7 3 9 2 11 4 13 6 15 8 0 10 14 12]);; no acaba

(main [6 1 2 3 0 4 11 7 12 15 10 5 9 8 13 14])

(main [15 0 3 7 1 2 6 14 4 9 5 10 8 12 11 13])



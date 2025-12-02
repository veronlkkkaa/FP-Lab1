(ns lab1.euler9)

;; вспомогательная функция для умножения коллекции (big-int friendly)
(defn product [coll]
  (reduce *' coll))

;;  1. Хвостовая рекурсия 
(defn tail-rec []
  (loop [a 1
         b 2]
    (if (> a 333)
      nil
      (let [c (- 1000 a b)]
        (cond
          (> b c)
          (recur (inc a) (+ (inc a) 1))

          (= (+ (* a a)
                (* b b))
             (* c c))
          (* a b c)

          :else
          (recur a (inc b)))))))

;; 2. Обычная рекурсия
(defn solve-rec
  ([] (solve-rec 1)) 
  ([a]
   (if (> a 333)
     nil
     (let [res (some (fn [b]
                       (let [c (- 1000 a b)]
                         (when (and (> c b)
                                    (= (+ (* a a)
                                          (* b b))
                                       (* c c)))
                           (* a b c))))
                     (range (inc a) 500))]
       (if res
         res
         (solve-rec (inc a)))))))



;; 3. Модульный подход

(defn all-triples []
  (for [a (range 1 334)
        b (range (inc a) 500)]
    [a b (- 1000 a b)]))

(defn pythagorean? [[a b c]]
  (and (> c b)
       (= (+ (* a a)
             (* b b))
          (* c c))))

(defn solve-modular []
  (->> (all-triples)      ; генерация
       (filter pythagorean?) ; фильтрация
       first
       product))          ; свёртка (reduce внутри product)

;; 4. Генерация при помощи map 
(def triples
  (->> (range 1 334)
       (mapcat (fn [a]
                 (map (fn [b]
                        [a b (- 1000 a b)])
                      (range (inc a) 500))))))

(defn solve-with-map []
  (->> triples
       (filter pythagorean?)   ; используем тот же предикат
       first
       product))

;; 5. Бесконечная ленивная последовательность 


(def lazy-triples
  (for [a (iterate inc 1)          
        b (iterate inc (inc a))    
        :let [c (- 1000 a b)]
        :while (pos? c)]           ;
    [a b c]))

(defn solve-lazy []
  (->> lazy-triples
       (filter pythagorean?)  
       first
       product))

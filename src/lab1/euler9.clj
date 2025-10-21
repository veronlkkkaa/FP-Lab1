(ns lab1.euler9
  (:require [clojure.string :as str]))

;; вспомогательная функция для умножения коллекции (big-int friendly)
(defn product [coll]
  (reduce *' coll))

;; 1. Хвостовая рекурсия
(defn tail-rec []
  (loop [a 1
         b 2]
    (when-not (> a 333)
      (let [c (- 1000 a b)]
        (cond
          (> b c)
          (recur (inc a) (inc (inc a)))

          (= (+ (* a a)
                (* b b))
             (* c c))
          (* a b c)

          :else
          (recur a (inc b)))))))

;; 2. Обычная рекурсия
(defn solve-rec []
  (loop [a 1
         b 2]
    (when-not (> a 333)
      (let [c (- 1000 a b)]
        (cond
          (> b c)
          (recur (inc a) (inc a))

          (= (+ (* a a)
                (* b b))
             (* c c))
          (* a b c)

          :else
          (recur a (inc b)))))))

;; 3. Модульный подход
(defn generate-triples []
  (for [a (range 1 334)
        b (range (inc a) 500)
        :let [c (- 1000 a b)]
        :when (and (> c b)
                   (= (+ (* a a)
                         (* b b))
                      (* c c)))]
    [a b c]))

(defn solve-modular []
  (->> (generate-triples)
       first
       (apply *)))

;; 4. Перебор общий источник данных для обеих реализаций
(def triples
  (->> (range 1 334)
       (mapcat (fn [a]
                 (map (fn [b]
                        (let [c (- 1000 a b)]
                          (when (and (> c b)
                                     (= (+ (* a a) (* b b))
                                        (* c c)))
                            [a b c])))
                      (range (inc a) 500))))
       (remove nil?)))

;; 4 Map
(defn solve-with-map []
  (let [triple (first triples)]
    (when triple
      (apply * triple))))

;; 5 Ленивая последовательность. используем тот же ленивый перебор triples, берём ту же тройку и считаем произведение
(defn solve-lazy []
  (let [[a b c] (first triples)]
    (when a
      (* a b c))))

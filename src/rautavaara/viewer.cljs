(ns rautavaara.viewer
  (:require [leramir.ast :as ast]
            [leramir.types.timed-value :as tv]
            [leramir.utils.math :as utils.math] 
            [leramir.core :as core]
            [rational.core :as r]))

(defn pitch-set [x]
  ;;eventually pitch/pitch-set, but that isn't stable at this time because the protocol implememntation isn't clj-safe
  ;;
  (cond (set? x)
        x
        (number? x)
        #{x}
        :else #{}))

(defn piano-roll [width height path-value-map]
  (let [pitches (sort (mapcat (comp pitch-set tv/value) (vals path-value-map)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch)))
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        nn->i (zipmap stave (range))
        nn->height (fn [nn]
                     (.log js/console (print-str nn))
                     (assert (int? nn))
                     (* (nn->i nn) height-per-stave))]
    (concat (for [[i nn] (partition 2 (interleave (range) stave))]
              [:rect
               {:stroke :gainsboro
                :stroke-width 0.5
                :fill (if (contains?
                           #{0 2 4 5 7 9 11}
                           (int (mod nn 12)))
                        :white
                        :whitesmoke)
                :x 0
                :y (nn->height nn)
                :width width
                :height height-per-stave}])
            (for [tv (vals path-value-map)
                  :let [pitch-set (pitch-set (tv/value tv))
                        color (:color (tv/attrs tv))]
                  nn pitch-set
                  :when nn]
              [:rect
               {:stroke :gainsboro
                :stroke-width 0.5
                :fill (or color :black)
                :x (rescale-tv-time (tv/start tv))
                :y (nn->height nn)
                :width (rescale-tv-time (tv/duration tv))
                :height height-per-stave}]))))

(defn bracket [from to height]
  (let [attrs {:stroke :black}]
    [:g
     [:line
      (merge
       attrs
       {:x1 from
        :y1 0
        :x2 to
        :y2 0})]
     [:line
      (merge
       attrs
       {:x1 from
        :y1 0
        :x2 from
        :y2 height})]
     [:line
      (merge
       attrs
       {:x1 to
        :y1 0
        :x2 to
        :y2 height})]]))

(defn translate [x y & children]
  (into [:g {:transform (str "translate(" x " " y ")")}] children))

(defn vizualize-era [era]
  (let [path-value-map (core/era->path-value-map era)
        width 800
        per-rung 10
        rung-padding 5
        rung-total (+ rung-padding per-rung)
        piano-roll-height 300
        metas (into {} (filter (fn [[path tv]] (keyword? (tv/value tv))) path-value-map)) ;; todo -> rewrite with specter
        max-count (apply max (map count (keys metas)))
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        margin-height (* rung-total (inc max-count))
        total-height (+ margin-height piano-roll-height)]
    [:svg
     {:width width
      :height total-height}
     (for [[path tv] metas]
       (translate 0 (* rung-total (count path))
                  [:text {:dominant-baseline :hanging
                          :x (rescale-tv-time (tv/start tv))
                          :font-family :helvetica
                          :font-size 8
                          :y 0}
                   (str (name (tv/value tv)) (print-str path))]
                  (bracket (rescale-tv-time (tv/start tv))
                           (rescale-tv-time (tv/end tv))
                           per-rung)))
     (translate 0 margin-height
                (piano-roll width piano-roll-height path-value-map))]))

(defn root []
  [:<> 
   [:h1 "there there, it's not all bad"]
   (doall (vizualize-era [:era 2 [:era {:color "green"} 2 3] [2 3]]))
   [:br] 
   (doall (vizualize-era [:era 2 2]))
   [:br]
   "hi is what I say to my friends. They don't neccessarily love it, but they put up with it."
   [:svg]])


(comment 
  (vizualize-era [[:graft {:color "green"} 2] 4 [ 5]])
  
  (ast/standard-interpretation [1 2 3])
  )
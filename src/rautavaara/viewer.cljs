(ns rautavaara.viewer
  (:require [leramir.ast :as ast]
            [leramir.types.timed-value :as tv]
            [leramir.utils.math :as utils.math]
            [leramir.core :as core]
            [react :as react]
            [reagent.core]
            [rational.core :as r]))

(defn spy [x] (.log js/console x) x)
(defn pitch-set [x]
  ;;eventually pitch/pitch-set, but that isn't stable at this time because the protocol implememntation isn't clj-safe
  ;;
  (cond (set? x)
        x
        (number? x)
        #{x}
        :else #{}))

(defn piano-roll* [width height path-value-map]
  (let [pitches (sort (mapcat (comp pitch-set tv/value) (vals path-value-map)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch)))
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        nn->i (zipmap stave (range))
        nn->height (fn [nn]
                     #_(.log js/console (print-str nn))
                     (assert (int? nn))
                     (* (nn->i nn) height-per-stave))]
    [:svg {:width width :height height}
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
                 :height height-per-stave}]))]))

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

(defn voice-tree [{:keys [era width]}]
  (let [path-value-map (core/era->path-value-map era)
        per-rung 10
        rung-padding 5
        rung-total (+ rung-padding per-rung) 
        metas (into {} (filter (fn [[path tv]] (keyword? (tv/value tv))) path-value-map)) ;; todo -> rewrite with specter
        max-count (apply max (map count (keys metas)))
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        total-height (* rung-total (inc max-count))]
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
     #_(translate 0 margin-height
                (piano-roll width piano-roll-height path-value-map))]))

(defn piano-roll [{:keys [era width height] :as attrs}] 
  [piano-roll* width height (core/era->path-value-map era)])

(defn resizable-div* [_component _attrs]
  (let [size (reagent.core/atom {:width 0 :height 300})
        resize-observer (js/ResizeObserver. (fn [entries _] 
                                              (when-let [entry (first entries)]
                                                (.log js/console (.-target entry))
                                                (let [dom-rect (.-contentRect entry)]
                                                  (spy entry)
                                                  (spy dom-rect)
                                                  (reset! size {:width (.-width dom-rect)
                                                                :height (spy (- (.-height dom-rect) 0 #_6.5))})))))]
    (fn [component attrs]
      (let [my-ref (react/useRef)]
        (react/useEffect
         (fn []
           (println my-ref)
           (when (.-current my-ref)
             (.observe resize-observer (.-current my-ref)))
           (fn []
             (.disconnect resize-observer)))
         (clj->js []))
        
        [:div.vertical-resize
         {:ref my-ref
          :style {:height (:height @size)}}
         [component (merge attrs @size)]]))))

(defn resizable-div [component attrs]
  [:f> resizable-div* component attrs])

(defn visualize-era [{:keys [era width] :as attrs}]
  [:<>
   [resizable-div voice-tree attrs]
   [resizable-div piano-roll attrs]])

(defn editor [era on-change]
  [visualize-era
   {:era era
    :on-change on-change}])

(defn root []
  (let [era (reagent.core/atom 
             [:era 2 3 #{4 5 6} [:heap 2
                                 [:era  2 3]
                                 [2 3]
                                 1 3 5 7 13]])]
    (fn []
      [:<>
       [:div.notebook
        [:h1 "UI ideas"]]
       [editor @era (fn [new-era]
                      (reset! era new-era))] 
       [:br]
       [:svg]])))


(comment
  ;; todo -- navigation
  ;; cross siblings - 'horizontal'
  ;; accross levels - 'vertical'

  ;; todo -- zooming
  ;; you can zoom up to the current level that you're at
  ;; moving to a sibling shifts the zoom to the sibling
  ;; moving to a parent shifts the zoom to the parent

  ;; todo - editing
  ;; slup
  ;; barf
  ;; moving pitch values up and down
  ;; move up/down by octave
  

  (resizable-div
   (vizualize-era [[:graft {:color "green"} 2] 4 [5]]))

  (ast/standard-interpretation [1 2 3]))
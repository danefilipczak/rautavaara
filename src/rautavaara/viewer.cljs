(ns rautavaara.viewer
  (:require [leramir.ast :as ast]
            [leramir.types.timed-value :as tv]
            [leramir.utils.math :as utils.math]
            [leramir.core :as core]
            [react :as react]
            [reagent.core]
            [clojure.pprint]
            [rational.core :as r]))

(defn pprint [x]
  [:pre
   (with-out-str (clojure.pprint/pprint x))])

(defn spy [x] (.log js/console x) x)
(defn pitch-set [x]
  ;;eventually pitch/pitch-set, but that isn't stable at this time because the protocol implememntation isn't clj-safe
  ;;
  (cond (set? x)
        x
        (number? x)
        #{x}
        :else #{}))

;;; begin move into core.ast

(defn era? [ast]
  (= ::leramir.ast/era (:type ast)))

(defn values* [result ast]
  (if (era? ast)
    (apply concat result (map (partial values* result) (:children ast)))
    (conj result ast)))

(defn values [ast]
  (values* [] ast))

(defn value [ast] (:value ast))

(defn end [ast] (r/+ (:start ast) (:duration ast)))

(defn start [ast] (:start ast))
(defn duration [ast] (:duration ast))
(defn attrs [ast] (:percolated-attrs ast))

(defn sub-path? [path-1 path-2]
  (= path-1 (vec (take (count path-1) path-2))))

(defn up [ast path]
  (when (> (count path) 0)
    (:path (get-in ast (leramir.ast/ast-path (pop path))))))

(defn down [ast path]
  (:path (get-in ast (leramir.ast/ast-path (conj path 0)))))

(defn left [ast path]
  (when (and (not-empty path) (> (peek path) 0))
    (:path (get-in ast (leramir.ast/ast-path (conj (pop path) (dec (peek path))))))))

(defn right [ast path]
  (when (not-empty path)
    (:path (get-in ast (leramir.ast/ast-path (conj (pop path) (inc (peek path))))))))

(defn nearest-era [ast path]
  (if (era? (get-in ast (leramir.ast/ast-path path)))
    ast
    (:path (get-in ast (leramir.ast/ast-path (pop path))))))

(comment

  (parent [13 2] (leramir.ast/standard-interpretation [1 2 3]))
  (pop [1 2 3 4]))

;;; end move into core.ast

(def highlight-color "#AADE91")

(defn piano-roll* [{:keys [width height ast active-path]}]
  (let [pitches (sort (mapcat (comp pitch-set value) (values ast)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch)))
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize end) (values ast))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        nn->i (zipmap stave (range))
        nn->height (fn [nn]
                     (assert (int? nn))
                     (* (nn->i nn) height-per-stave))]
    [:svg {:width width :height height}
     (concat (for [[i nn] (partition 2 (interleave (range) stave))]
               [:rect
                {:key nn
                 :stroke :gainsboro
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
             (for [v (values ast)
                   :let [pitch-set (pitch-set (value v))
                         active? (sub-path? active-path (:path v))
                         color (or (:color (attrs v)) (when active? highlight-color))]
                   nn pitch-set
                   :when nn]
               [:rect
                {:key (print-str (conj (:path v) nn))
                 :stroke (if active? :black :gainsboro)
                 :stroke-width 0.5
                 :fill (or color :black)
                 :x (rescale-tv-time (start v))
                 :y (nn->height nn)
                 :width (rescale-tv-time (duration v))
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

(defn voice-tree [{:keys [era width height]}]
  (let [path-value-map (core/era->path-value-map era)
        metas (into {} (filter (fn [[path tv]] (keyword? (tv/value tv))) path-value-map)) ;; todo -> rewrite with specter
        max-count (apply max (map count (keys metas)))
        per-rung (/ height (inc max-count))
        rung-padding 0#_5
        rung-total (+ rung-padding per-rung)
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        total-height height]
    [:svg
     {:width width
      :height total-height}
     (for [[path tv] metas]
       [:g {:key (print-str path)}
        (translate 0 (* rung-total (count path))
                   [:text {:dominant-baseline :hanging
                           :x (rescale-tv-time (tv/start tv))
                           :font-family :helvetica
                           :font-size 8
                           :y 0}
                    (str (name (tv/value tv)) (print-str path))]
                   (bracket (rescale-tv-time (tv/start tv))
                            (rescale-tv-time (tv/end tv))
                            per-rung))])
     #_(translate 0 margin-height
                  (piano-roll width piano-roll-height path-value-map))]))

(defn piano-roll [{:keys [era width height] :as attrs}]
  [piano-roll*
   (merge attrs {:ast (leramir.ast/standard-interpretation era)})])

(defn resizable-div* [initial-height _component _attrs]
  (let [min-height 100
        size (reagent.core/atom {:width 0 :height initial-height})
        resize-observer (js/ResizeObserver. (fn [entries _]
                                              (when-let [entry (first entries)]
                                                (let [dom-rect (.-contentRect entry)]
                                                  entry
                                                  dom-rect
                                                  (reset! size {:width (.-width dom-rect)
                                                                :height (.-height dom-rect)})))))]
    (fn [_initial-height component attrs]
      (let [my-ref (react/useRef)]
        (react/useEffect
         (fn []
           (when (.-current my-ref)
             (.observe resize-observer (.-current my-ref)))
           (fn []
             (.disconnect resize-observer)))
         (clj->js []))

        [:div.vertical-resize
         {:ref my-ref
          :style {:height (:height @size)
                  :min-height min-height}}
         [component (merge attrs @size)]]))))

(defn resizable-div [initial-height component attrs]
  [:f> resizable-div* initial-height component attrs])

(defn visualize-era [{:keys [era width] :as attrs}]
  [:<>
   [resizable-div 100 voice-tree attrs]
   [resizable-div 300 piano-roll attrs]])

(defn editor [era on-change]
  (let [active-path (reagent.core/atom [])]
    (fn [era on-change]
      (let [ast (leramir.ast/standard-interpretation era)
            current-node (get-in ast (leramir.ast/ast-path @active-path))]
        [:<>
         [visualize-era
          {:era era
           :on-change on-change
           :active-path @active-path}]
         [:button
          {:on-click #(when-let [d (down ast @active-path)]
                        (spy d)
                        (reset! active-path d))}
          "down"]
         [:button
          {:on-click #(when-let [d (up ast @active-path)]
                        (spy d)
                        (reset! active-path d))}
          "up"]
         [:button
          {:on-click #(when-let [d (left ast @active-path)]
                        (spy d)
                        (reset! active-path d))}
          "left"]
         [:button
          {:on-click #(when-let [d (right ast @active-path)]
                        (spy d)
                        (reset! active-path d))}
          "right"]
         #_[pprint current-node]]))))

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
  ;; the basic ops are up, down, left right

  ;; todo -- zooming
  ;; you can zoom up to the current level that you're at
  ;; moving to a sibling shifts the zoom to the sibling
  ;; moving to a parent shifts the zoom to the parent

  ;; value editing - read string a str, number, or set

  ;; cycle era type

  ;; todo - editing
  ;; slup
  ;; barf


  ;; transpose by semitone
  ;; variation - transpose by octave 


  (resizable-div
   (vizualize-era [[:graft {:color "green"} 2] 4 [5]]))

  (ast/standard-interpretation [1 2 3]))
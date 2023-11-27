(ns rautavaara.viewer
  (:require [clojure.pprint]
            [leramir.ast :as ast]
            [leramir.core :as core]
            [leramir.types.timed-value :as tv]
            [leramir.utils.math :as utils.math]
            [rational.core :as r]
            [react :as react]
            [reagent.core]))

(defn pprint [x]
  [:pre
   (with-out-str (clojure.pprint/pprint x))])

(defn spy [x]
  (.log js/console (print-str x))
  x)
(defn pitch-set [x]
  ;;eventually pitch/pitch-set, but that isn't stable at this time because the protocol implememntation isn't clj-safe
  ;;
  (cond (set? x)
        x
        (number? x)
        #{x}
        :else #{}))

(defn nearest-era [ast path]
  (if (ast.era? (get-in ast (leramir.ast/ast-path path)))
    (get-in ast (leramir.ast/ast-path path))
    (get-in ast (leramir.ast/ast-path (pop path)))))

(def highlight-color "#AADE91")
(def contrast-color #_:lightgrey :whitesmoke)

(defn piano-roll* [{:keys [width height ast active-path]}]
  (let [pitches (sort (mapcat (comp pitch-set ast/value) (ast/values ast)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch)))
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize ast/end) (ast/values ast))))
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
                         contrast-color)
                 :x 0
                 :y (nn->height nn)
                 :width width
                 :height height-per-stave}])
             (let [values (ast/values ast)
                   {active true non-active false} (group-by #(ast/sub-path? active-path (:path %)) values)
                   draw-things (fn [things active?]
                                 (for [v things
                                       :let [pitch-set (pitch-set (ast/value v))
                                             color (or (:color (ast/attrs v)) (when active? highlight-color))]
                                       nn pitch-set
                                       :when nn]
                                   [:rect
                                    {:key (print-str (conj (:path v) nn))
                                     :stroke (if active? :black :gainsboro)
                                     :stroke-width 0.5
                                     :fill (or color :black)
                                     :x (rescale-tv-time (ast/start v))
                                     :y (nn->height nn)
                                     :width (rescale-tv-time (ast/duration v))
                                     :height height-per-stave}]))]
               [:<>
                (draw-things non-active false)
                (draw-things active true)]))]))

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

(defn extract-voices [node]
  (apply
   merge-with
   concat
   {(:voice node) [node]}
   (map extract-voices (ast/children node))))

(defn lexicographic-comparator [vector-a vector-b]
  (loop [v1 vector-a
         v2 vector-b]
    (cond
      (empty? v1) (if (empty? v2) 0 -1)
      (empty? v2) 1
      :else
      (let [cmp (compare (first v1) (first v2))]
        (if (zero? cmp)
          (recur (rest v1) (rest v2))
          cmp)))))

(defn jared [{:keys [era width height active-path]}]
  (def era era)
  (let [nodes-by-voice (extract-voices (leramir.ast/standard-interpretation era))
        ->range (fn [node] [(ast/start node) (ast/end node)])
        sorted-voices (sort lexicographic-comparator (keys nodes-by-voice))
        per-voice (/ height (count sorted-voices))
        last-end (->> nodes-by-voice
                      vals
                      (map (fn [nodes]
                             (map ->range nodes)))
                      (mapcat (fn [ranges]
                                (map (comp r/realize second) ranges)))
                      sort
                      last)
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        total-height height]
    [:svg
     {:width width
      :height total-height}
     (doall
      (for [[i voice] (partition 2 (interleave (range) sorted-voices))
            :let [nodes (get nodes-by-voice voice)]
            node nodes
            :let [[start end :as range] (->range node)
                  active? (ast/sub-path? active-path (:path node))]]
        [:g {:key (print-str [voice (:path node)])}
         (translate 0 (* per-voice i)
                    (when active?
                      [:rect
                       {:key (print-str [range voice])
                        #_#_:stroke (if active? :black :gainsboro)
                        :stroke-width 0.5
                        :fill highlight-color
                        :x (rescale-tv-time start)
                        :y 0
                        :width (- (rescale-tv-time end) (rescale-tv-time start))
                        :height per-voice}])
                    [:text {:dominant-baseline :hanging
                            :x (rescale-tv-time start)
                            :font-family :helvetica
                            :font-size 8
                            :y 0}
                     #_(str voice i)
                     (str (:path node))]
                    (bracket (rescale-tv-time start) 
                             (rescale-tv-time end)
                             per-voice))]))]))

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

(defn inspector [{:keys [era active-path width] :as attrs}]
  (let [ast (leramir.ast/standard-interpretation era)
        parent-path (cond-> active-path (not-empty active-path) pop)
        parent-era (get-in ast (leramir.ast/ast-path parent-path))]
    [:div {:style {:width "100%"
                   :height "26px"
                   :position :relative}}
     (doall
      (for [child (ast/children parent-era)
            :let [display (cond
                            (ast.era? child) (:tag child)
                            (set? (ast/value child)) #{}
                            :else (ast/value child))
                  active? (= active-path (:path child))]]
        [:div
         {:style {:position :absolute
                  :top 0
                  :width (str (* 100 (r/realize (ast/duration child))) "%")
                  :left (str (* 100 (r/realize (ast/start child))) "%")
                  :text-decoration (when active? "underline")}}
         [:span
          {:style {:font-family "monospace" :font-size "12px"}}
          (print-str display)]]))]))

(defn visualize-era [{:keys [era width] :as attrs}]
  [:<>
   #_[resizable-div 100 voice-tree attrs]
   [resizable-div 100 jared attrs]
   ;; this inspector should instead be an excel-style single line read/edit window. 
   ;; the children of eras should be represented by ...n where n is the count of the children. clicking on that will expand it one level. 
   ;; trickily, the view should also be edit-able so that values can be manually entered.
   #_[inspector attrs]
   [resizable-div 300 piano-roll attrs]])

(defn buttons [ast active-path]
  [:<>
   [:button
    {:on-click #(when-let [d (ast/down ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/down ast @active-path))}
    "down"]
   [:button
    {:on-click #(when-let [d (ast/up ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/up ast @active-path))}
    "up"]
   [:button
    {:on-click #(when-let [d (ast/left ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/left ast @active-path))}
    "left"]
   [:button
    {:on-click #(when-let [d (ast/right ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/right ast @active-path))}
    "right"]])

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
         [buttons ast active-path]
         [:pre (print-str @active-path)]
         #_[pprint current-node]]))))

(defn root []
  (let [era1 (reagent.core/atom
              [:era 2 3 [:chain #{4 5 6} #{4 5 6}] [:heap 2
                                                    [:era  2 3]
                                                    [2 3]
                                                    1 3 5 7 13]])
        era2 (reagent.core/atom
              [:chain 1 2 3 4])
        era3 (reagent.core/atom
              [:era 2 3 [:chain #{4 5 6} #{4 5 6}] [:heap 2
                                                    [:era  2 3]
                                                    [2 3]
                                                    1 3 5 7 13]])]
    (fn []
      [:<>
       [:<>
        [:div.notebook
         [:h1 "UI ideas"]
         [:p "hello you tricky devil, is this thing on?"]]
        [editor @era1 (fn [new-era]
                        (reset! era new-era))]
        [:pre (print-str @era1)]]
       [:<>
        [:div.notebook]
        [editor @era2 (fn [new-era]
                        (reset! era new-era))]
        [:pre (print-str @era2)]]
       [:<>
        [:div.notebook]
        [editor @era3 (fn [new-era]
                        (reset! era new-era))]
        [:pre (print-str @era3)]
        [:div.notebook
         [:h2 "Notes for the future"]
         [:p "If we can go no further right, try to go down"]
         [:p "If we can go no further left, try to go up"]]]])))


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
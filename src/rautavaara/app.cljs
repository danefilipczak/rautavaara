(ns rautavaara.app
  (:require [reagent.dom :as r.dom]))

(defn app []
  "hi there, hello there")

(defn mount []
  (r.dom/render [app] (js/document.getElementById "root")))

(defn ^:dev/after-load re-render []
  (mount))


(defn init [] 
  (println "Hello World") 
  (mount))

(comment
  
  (js/alert "hi")
  )
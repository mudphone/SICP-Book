(ns app.elements
  (:require
   [hoplon.core
     :as h
     :include-macros true]))

(defn a-item
  ([code-str]
   (a-item code-str nil))
  ([code-str answer]
   (h/li (h/pre (h/code code-str) answer))))

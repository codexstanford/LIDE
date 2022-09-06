(ns lide.editor
  (:require
   [re-frame.core :as rf]))

;; VS Code interop

(rf/reg-fx
 ::tell-vs-code
 (fn [[vs-code message]]
   (when vs-code
     (. vs-code
        postMessage
        (clj->js message)))))

(rf/reg-event-fx
 ::focus-range
 (fn [cofx [_ [start end]]]
   {:fx [[::tell-vs-code [(-> cofx :db :vs-code)
                          {:type "focusRange"
                           :range {:startPosition start
                                   :endPosition end}}]]]}))

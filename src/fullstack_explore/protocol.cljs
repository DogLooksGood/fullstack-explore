(ns fullstack-explore.protocol)

(defmulti get-query
  (fn [component] component))

(defmethod get-query :default
  [component]
  (throw (ex-info "Query not found!" {:component component})))

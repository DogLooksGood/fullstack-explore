(ns fullstack-explore.front
  (:require-macros fullstack-explore.front)
  (:require [reagent.core :as r]))

(defonce app-state (r/atom {}))

(defn is-ident? [x]
  (and (vector? x) (keyword? (first x))))

(defn get-query [comp-or-meta]
  (when-let [q (:query (meta comp-or-meta))]
    (q)))

(defn get-ident-key [component]
  (:ident (meta component)))

(defn component->meta [component]
  {:query (get-query component)
   :ident (get-ident-key component)})

(defn make-ident [ident-key entity]
  (if (keyword? ident-key)
    (when-let [v (get entity ident-key)]
      [ident-key v])
    ident-key))

(defn get-ident [component entity]
  (when-let [f (get-ident-key component)]
    (if (keyword? f)
      (make-ident f entity)
      f)))

(defn join-attr? [attr]
  (map? attr))

(defn join-attr->ident [attr]
  (:ident (meta attr)))

(defn join-attr->inline? [attr]
  (:inline? (meta attr)))

(defn merge-entity
  [state ident-key query entity]
  (cond
    (map? entity)
    (let [ident (make-ident ident-key entity)
          state (assoc-in state ident entity)]
      (reduce (fn [st attr]
                (if (join-attr? attr)
                  (let [idt-k (join-attr->ident attr)
                        k     (ffirst attr)
                        q     (second (first attr))
                        sub   (get entity k)
                        sub-type (cond
                                   ;; is a entity
                                   (map? sub)
                                   :entity

                                   ;; is a list of entities
                                   (and (vector? sub)
                                     (map? (first sub)))
                                   :entities

                                   ;; is a ident or list of idents
                                   :else
                                   :idents)

                        idt-or-idts
                        (case sub-type
                          :entity (make-ident idt-k sub)
                          :entities (mapv #(make-ident idt-k %) sub)
                          :idents sub)]
                    (cond-> st
                      idt-or-idts
                      (update-in ident assoc k idt-or-idts)

                      (#{:entity :entities} sub-type)
                      (merge-entity idt-k q sub)))
                  st))
        state
        query))

    :else
    (reduce #(merge-entity %1 ident-key query %2)
      state
      entity)))

(defn normalize [component-or-meta tree]
  (let [[ident-key query]
        (if (map? component-or-meta)
          [(:ident component-or-meta)
           (:query component-or-meta)]
          [(get-ident-key component-or-meta)
           (get-query component-or-meta)])]
    (merge-entity {} ident-key query tree)))

(defn normalize-merge! [component-or-meta tree]
  (let [[ident-key query]
        (if (map? component-or-meta)
          [(:ident component-or-meta)
           (:query component-or-meta)]
          [(get-ident-key component-or-meta)
           (get-query component-or-meta)])]
    (swap! app-state
      merge-entity
      ident-key
      query
      tree)))

(defn fetch
  [db ident query]
  (let [entity (get-in db ident)
        xform-q->val
        (map (fn [attr]
               (cond
                 (keyword? attr)
                 (find entity attr)

                 (:inline? (meta attr))
                 (let [[k q] (first attr)
                       idt-or-idts (get entity k)
                       val
                       (if (is-ident? idt-or-idts)
                         (fetch db idt-or-idts q)
                         (mapv #(fetch db % q) idt-or-idts))]
                   [k val])

                 :else
                 (find entity (ffirst attr)))))]
    (into {} xform-q->val query)))

(defn fetch-db
  [ident query]
  (fetch @app-state ident query))

(comment
  (fetch
    {:person/id {1 {:person/name "mike"
                    :person/id 1
                    :person/pets [[:pet/id 1]]}}
     :pet/id {1 {:pet/name "lily"
                 :pet/id 1}}}

    [:person/id 1]

    [:person/name ^{:inline? true} {:person/pets [:pet/id :pet/name]}]))

(ns fullstack-explore.front
  (:require-macros fullstack-explore.front)
  (:require [reagent.core :as r]
            [datascript.core :as d]))

(defonce app-state (r/atom {}))

(defn is-ident? [x]
  (and (vector? x) (keyword? (first x))))

(defn get-query [component]
  (when-let [f (:query (meta component))]
    (f)))

(defn get-ident-key [component]
  (:ident (meta component)))

(defn component->meta [component]
  {:query (get-query component)
   :ident (get-ident-key component)})

(defn make-ident [ident-key entity]
  (when-let [v (get entity ident-key)]
    [ident-key v]))

(defn get-ident [component entity]
  (when-let [f (get-ident-key component)]
    (when (keyword? f)
      (make-ident f entity))))

(defn join-attr? [attr]
  (map? attr))

(defn join-attr->ident [attr]
  (:ident (meta attr)))

(defn join-attr->inline? [attr]
  (:inline? (meta attr)))

(defn merge-entity
  [state ident-key query entity]
  (if (map? entity)
    (let [ident (make-ident ident-key entity)
          state (assoc-in state ident entity)]
      (reduce (fn [st attr]
                (if (join-attr? attr)
                  (let [idt-k (join-attr->ident attr)
                        k (ffirst attr)
                        q (second (first attr))
                        new-sub-ent (get entity k)
                        idt-or-idts
                        (if (vector? new-sub-ent)
                          (mapv #(make-ident idt-k %) new-sub-ent)
                          (make-ident idt-k new-sub-ent))]
                    (cond-> st
                      idt-or-idts
                      (update-in ident assoc k idt-or-idts)

                      new-sub-ent
                      (merge-entity idt-k q new-sub-ent)))
                  st))
        state
        query))
    (reduce #(merge-entity %1 ident-key query %2)
      state
      entity)))

(defn normalize [component tree]
  (merge-entity
    {}
    (get-ident-key component)
    (get-query component)
    tree))

(defn normalize-merge! [component tree]
  (swap! app-state
    merge-entity
    (get-ident-key component)
    (get-query component)
    tree))

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

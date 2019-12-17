(ns fullstack-explore.front
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs]))

;;; Generate EQL query from argument destructing.
;;;

(defn arg->query+ident
  ([arg]
   (arg->query+ident {:query [] :ident nil} arg))
  ([rst arg]
   (reduce-kv
     (fn [r k v]
       (cond
         (qualified-keyword? k)
         (let [ns-str (namespace k)
               attrs
               (map (fn [x]
                      (let [attr (keyword ns-str (str x))]
                        (if-let [tag (:tag (meta x))]
                          `(if (map? ~tag)
                             (with-meta
                               {~attr (:query ~tag)}
                               {:ident (:ident ~tag)
                                :inline? true})
                             (with-meta
                               {~attr (fullstack-explore.front/get-query ~tag)}
                               {:ident (fullstack-explore.front/get-ident-key ~tag)}))
                          (if-let [q (:query (meta x))]
                            (let [idt (:ident (meta x))]
                              `(with-meta
                                 {~attr ~q}
                                 {:ident ~idt
                                  :force true}))
                            attr))))
                 v)
               idt-k (first (filter #(:ident (meta %)) v))]
           (cond-> r
             (seq attrs)
             (update :query into attrs)

             idt-k
             (assoc :ident (keyword ns-str (str idt-k)))))

         (symbol? k)
         (cond-> r
           true
           (update :query conj v)

           (:ident (meta k))
           (assoc :ident v))

         :else
         r))
     rst
     arg)))

(comment
  (arg->query+ident
    '{:a/keys [^:ident a ^B b ^{:query [:c/a]} c]}))

(defn defc* [sym binding & body]
  (let [arg   (first binding)
        {:keys [ident query]} (arg->query+ident arg)
        qsym  (symbol (name (ns-name *ns*)) (name sym))
        idt-sym (gensym "idt")]
    `(let [query# (fn [] ~query)
           ident# ~ident
           render#
           (fn ~sym
             [prop#]
             (if (fullstack-explore.front/is-ident? prop#)
               (let [idt# prop#
                     reaction# (reagent.ratom/reaction
                                 (fullstack-explore.front/fetch-db
                                   idt#
                                   ~query))]
                 (fn [~idt-sym]
                   (let [~(first binding) @reaction#]
                     ~@body)))
               (let [~(first binding) prop#]
                 ~@body)))]
       (def ~sym
         (with-meta render#
           {:query query#
            :ident ident#})))))

(defmacro defc [sym binding & body]
  (apply defc* sym binding body))

(comment
  (clojure.pprint/pprint
    (defc* 'person '[{:person/keys [^:ident id name]} ident]
      '[:div
        [:div "id:" id]
        [:div "name:" name]])))

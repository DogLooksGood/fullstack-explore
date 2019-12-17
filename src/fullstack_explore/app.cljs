(ns fullstack-explore.app
  (:require [reagent.core :as r]
            [fullstack-explore.front :as front :refer [defc get-query get-ident normalize-merge! normalize]]
            [goog.dom :as gdom]
            [datascript.core :as d]))

(defn make-older
  "age + 1"
  [id]
  (swap! front/app-state update-in [:pet/id id :pet/age] inc))

(defc pet
  [{:pet/keys [^:ident id name age]
    :as props}]
  (.log js/console "render pet" props)
  [:li (str id) " Pet: " name "," age "yrs old"
   [:button {:on-click (partial make-older id)} "Older"]])

(def pet-meta (front/component->meta pet))

(def account-meta
  {:query [:account/id :account/balance]
   :ident :account/id})

(defn add-money
  "add by 100"
  [id]
  (swap! front/app-state update-in [:account/id id :account/balance]
    + 100))

(defc person
  [{:person/keys [^:ident id name age ^pet pets ^account-meta account]
    :as props}]
  (.log js/console "render person" id pets)
  [:div
   [:div "ID:" id]
   [:div "Name: " name]
   [:div "Age: " age]
   [:div "Balance: $" (:account/balance account)
    [:button {:on-click (partial add-money (:account/id account))}
     "charge!"]]
   [:div "Pets"
    [:ul
     (for [p pets]
       ^{:key p}
       [pet p])]]])

(defn root []
  [:div
   [person [:person/id 1]]
   [person [:person/id 2]]])

(comment

  (get-query message)

  (meta (nth (get-query message) 2))

  (meta (nth (get-query person) 3))

  (meta person)

  (get-query person)

  (get-ident person {:person/name 1})

  (get-ident pet {:pet/id 1 :pet/name "sofia"})

  (normalize cat
    [{:person/id 1
      :person/name "Mike"
      :person/pets
      [{:pet/id 1 :pet/name "sofia"}]}
     {:person/id 2
      :person/name "Lucy"}])

  (cljs.pprint/pprint
    (normalize person
      [{:person/id 1
        :person/name "Mike"
        :person/pets
        [{:pet/id 1 :pet/name "sofia"}]}
       {:person/id 2
        :person/name "Lucy"}])))

(defn mount []
  (r/render [root] (gdom/getElement "app")))

(defn init-data
  []
  (normalize-merge! person
    [{:person/id 1
      :person/name "Mike"
      :person/age 28
      :person/pets
      [{:pet/id 1 :pet/name "lucy" :pet/age 10}
       {:pet/id 2 :pet/name "lily" :pet/age 6}]
      :person/account
      {:account/id 1 :account/balance 200}}
     {:person/id 2
      :person/name "Jack"
      :person/age 26
      :person/pets
      [{:pet/id 3 :pet/name "bob" :pet/age 3}]
      :person/account
      {:account/id 2 :account/balance 500}}]))

(defn init []
  (init-data)
  (mount))

(defn after-load [] (mount))

(comment
  (swap! front/app-state assoc-in [:pet/id 1 :pet/name] "lucia"))

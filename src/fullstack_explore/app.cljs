(ns fullstack-explore.app
  (:require [reagent.core :as r]
            [fullstack-explore.front :as front
             :refer [defc get-query get-ident normalize-merge!
                     normalize component->meta]]
            [goog.dom :as gdom]))

(def init-data
  [{:person/id 1
    :person/name "Mike"
    :person/age 28
    :person/pets
    [{:pet/id 1 :pet/name "lucy" :pet/age 10}
     {:pet/id 2 :pet/name "lily" :pet/age 6}]
    :person/account
    {:account/id 1
     :account/balance 500}}
   {:person/id 2
    :person/name "Jack"
    :person/age 26
    :person/pets
    [{:pet/id 3 :pet/name "bob" :pet/age 3}]
    :person/account
    {:account/id 2
     :account/balance 500}}])

(def account-meta
  {:query [:account/id :account/balance]
   :ident :account/id})

(defc pet
  [{:pet/keys [^:ident id name age]}]
  [:li
   [:div "id: " id]
   [:div "name: " name]
   [:div "age: " age]])

(defn add-money
  [{:account/keys [id]}]
  (swap! front/app-state update-in [:account/id id :account/balance] + 100))

(defc person
  [{:person/keys [^:ident id name age
                  ^pet pets
                  ^account-meta account]}]
  [:div
   [:div "id: " id]
   [:div "name: " name]
   [:div "age: " age]
   [:div "balance: $" (:account/balance account)
    [:button {:on-click (partial add-money account)} "Charge!"]]
   [:div "pets:"
    [:ul
     (for [p pets]
       ^{:key p}
       [pet p])]]])

(defn root []
  [:div "Peoples >>"
   [person [:person/id 1]]
   [person [:person/id 2]]])

(defn mount []
  (r/render [root] (gdom/getElement "app")))

(defn init []
  (normalize-merge! person init-data)
  (mount))

(.log js/console @front/app-state)

(defn after-load [] (mount))

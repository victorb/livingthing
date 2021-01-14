# livingthing

A once-in-a-lifetime REPL. Keep it alive, but do improve it. We're all in the same REPL.

Since the rules and state changes constantly, you're better off looking at
https://livingthing.club to see the latest updates.

### Running livingthing locally

Have leiningen installed. Run `lein repl`. Inside the REPL, execute `(-main)`

## My proposed changes

### Humanize the timestamps

```
(defn unix-epoch-to-human [t]
  (java.time.Instant/ofEpochMilli
    (* 1000 t)))
```

```
(defn $command-list-item [{:keys [execution-results
                                  executed-at from
                                  cmd from votes]}]
  [:div
   [:strong
    "From user user "
    from
    " at "
    (unix-epoch-to-human executed-at)]
   [:div "Received " votes " votes"]
   [:pre cmd]
   [:div "Output"]
   [:pre (str execution-results)]])
```

```
(defn $wrapper []
  [:div#wrapper
   [:h2
    "Current server time: "
    (-> @app-state :current-time (unix-epoch-to-human))]
   ($pending-vote-list)
   ($submit-command)
   [:div
    ($section-header "How LivingThing works")
    [:div
     {:style "font-family: serif; max-width: 700px;"}
     intro-message]]
   ($pending-list)
   ($command-list)])
```

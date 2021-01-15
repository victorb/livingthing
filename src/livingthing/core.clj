(ns livingthing.core
  (:require
    [cemerick.pomegranate :refer [add-dependencies]]
    [org.httpkit.server :as httpkit]
    [hiccup.core :as hiccup]
    [hiccup.page :as hiccup-page]
    [ring.middleware.params :refer [wrap-params]]
    [pandect.algo.sha1 :refer [sha1]]
    [clojure.pprint :refer [pprint]]
    [glow.core :refer [highlight-html generate-css]])
  (:import java.security.SecureRandom)
  (:gen-class))

;; This is the initial source code for LivingThing
;; Why initial? Because each run of LivingThing is different and alive
;; It's the antithesis of stateless

;; Hence the structure is a bit weird. If you're reading this, is because
;; you're trying to understand it to hopefully be able to change it. So
;; code here is both illustrative, and concrete. Or at least it's trying to be.

;; Function to add dependencies to the current classpath without
;; restarting the session. Thanks to clj-commons/pomegranate, this becomes
;; easy to handle.
(defn add-deps [deps]
  (add-dependencies :coordinates deps
                    :repositories (merge cemerick.pomegranate.aether/maven-central
                                         {"clojars" "https://clojars.org/repo"})))

(comment
  ;; This is how external deps can be loaded. You can find dependencies at clojars.org
  (add-deps '[[http-kit "2.5.0"]
              [hiccup "1.0.5"]
              [ring/ring-core "1.8.2"]
              [pandect "0.6.1"]])

  ;; After adding them, we need to require and specify what to call them
  ;; Never do this add-deps + manual require in real production Clojure code, use `ns`
  ;; It's only because we're cool kids we're doing it this hacky, improper way
  (require '[org.httpkit.server :as httpkit])
  (require '[hiccup.core :as hiccup])
  (require '[hiccup.page :as hiccup-page])
  (require '[ring.middleware.params :refer [wrap-params]])
  (require '[pandect.algo.sha1 :refer [sha1]]))

;; We start out by defining our application state. Only one of this exist and
;; if the repl for some reason restart, we lose it.
;; We're using defonce only because it's easier when running LivingThing locally
;; and reloading the file without loosing state
(defonce app-state
  (atom {;; unix timestamp (server) with current time
         :current-time nil
         ;; Which round we're at. One round = one execution
         :current-round 1
         ;; Commands that can be elected this round
         ;; key = :user
         ;; value = :command
         :pending-commands {}
         ;; Users who've voted this time
         ;; key = :user
         ;; value = :user who has pending-command
         :pending-votes {}
         ;; Shorthand for calculating the current standings between the commands
         ;; Basically pending-votes but the key is how many voted, value is what command
         :current-tally []
         ;; Commands that has been executed before,
         ;; {:command "..."
         ;;  :when LocalDateTime
         ;;  :from-user "..."}
         :executed-commands []}))

;;;; Vars controlling different things in LivingThing

;; How often should we select a winner and execute the command?
(def round-duration-seconds 10)

;; Max amount of characters for command
(def max-size 100)


;; Obfuscation of the IP addresses need a salt to be prefixed to the IP
;; Will only be kept in memory. Hope that no one tries to output this as a command
;; and if they do, hope someone puts a different command or votes for others.
(def salt
  (let [seed (byte-array 1024)]
    (.nextBytes (SecureRandom.) seed)
    seed))

;; Takes a string, concats with the salt and hashes it. Same input gives the same output,
;; granted `salt` is still the same
(defn hash-str [s]
  (sha1 (str salt "." s)))

;; Alternative implementation of hash-str which always gives a different result
;; Useful for testing voting as each command has it's own ID, even if the IP
;; is the same
;; (defn hash-str
;;   (let [seed (byte-array 1024)]
;;     (.nextBytes (SecureRandom.) seed)
;;     (sha1 (str seed))))

;; Get the current unix timestamp in seconds
(defn now []
  (quot (System/currentTimeMillis) 1000))

;; Returns a list of the current votes, with the winner being at index 0
(defn calculate-tally []
  (let [self-votes (reduce
                     (fn [acc [k v]]
                       (assoc acc k k))
                     {}
                     (:pending-commands @app-state))
        votes-to-count (merge (:pending-votes @app-state)
                              self-votes)]
    (reverse
      (sort-by
        :votes
        (map
          (fn [[id received-votes]]
            {:cmd (get-in @app-state [:pending-commands id])
             :from id
             :votes (count received-votes)})
          (group-by second votes-to-count))))))

;; Resets all the attributes in the app-state to the default state
(defn reset-pending-round! []
  (swap! app-state assoc :pending-commands {}
                         :pending-votes {}
                         :current-tally []))

;; Get the current winner based on a calculated tally
(defn get-winner []
  (->> (calculate-tally)
       (first)))

;; This piece of code wraps a string in (do) and then evalutes it
;; as clojure code. Don't ever do this in production kids. If you do,
;; ensure you don't pass user-provided input into it. If you end up having
;; to pass user-provided input into it, make sure you have some voting system
;; around it, because people are nice, surely.
(defn evaluate-string
  [s]
  (try
    (eval
      (read-string (str "(do" s ")")))
    (catch Exception err
      err)))

;; Takes a winner, executes it's command and put the output + command into
;; :executed-commands for future viewing. After it also resets the current round
(defn execute-and-declare-winner [{:keys [cmd from votes]
                                   :as winner}]
  (swap! app-state
         update
         :executed-commands
         conj
         (merge
           {:executed-at (:current-time @app-state)
            :execution-results (str (evaluate-string cmd))}
           winner))
  (swap! app-state update :current-round inc)
  (reset-pending-round!))

;; Checks if the current time is a time where we need to select a winner.
(defn time-to-select-winner? []
  (= 0
     (mod
       (:current-time @app-state)
       round-duration-seconds)))

;; Should be called once a second to determine if we need to select a winner,
;; and if we need to, select it.
(defn tick-once! []
  (swap! app-state assoc :current-time (now)
                         :current-tally
                         (calculate-tally))
  (when (time-to-select-winner?)
    (when-let [winner (get-winner)]
      (execute-and-declare-winner winner))))

;; In order to control something executing every X seconds, we
;; create an atom to keep a future in. Inside the future, we create an infinitive
;; loop where for each iteration, we wait for X ms. After that, call tick-once!
;; Storing the future in a atom allows us to cancel it if needed for local
;; development
(defonce server-ticker (atom nil))

;; Never-ending future that calls tick-once! once per second
(defn start-server-ticker! []
  (reset! server-ticker
    (future
      (while true
        (Thread/sleep 1000)
        (tick-once!)))))

;; Stops and cancels the current server-ticker
(defn stop-server-ticker! []
  (future-cancel @server-ticker)
  (reset! server-ticker nil))

;; Here comes bunch of HTML elements in the form of functions. No explanation required
(defn $section-header [label]
  [:h3
    {:style "color: #333;"}
    label])

(defn $link [title url]
  [:a
   {:target "_blank"
    :href url}
   title])

(defn $command-list-item [{:keys [execution-results
                                  executed-at from
                                  cmd from votes]}]
  [:div
   [:strong
    "From user user "
    from
    " at "
    executed-at]
   [:div "Received " votes " votes"]
   [:pre cmd]
   [:div "Output"]
   [:pre (str execution-results)]])

(defn $command-list []
  [:div
   ($section-header "Executed commands (latest first):")
   [:div
     {:style "max-height: 500px; overflow: scroll;"}
     (map
       $command-list-item
       (reverse
         (:executed-commands @app-state)))]])

;; Figures out if the ID behind this request already voted or not
(defn has-voted? [req]
  (let [ip (:remote-addr req)
        id (hash-str ip)]
    (boolean
        (get-in @app-state [:pending-votes id]))))

(defn $pending-vote-list [req]
  [:div
   ($section-header "Current Vote Tally:")
   (map
     (fn [{:keys [cmd from votes]}]
       [:div
        [:div votes " votes for"]
        [:code
         {:onClick "window.getSelection().selectAllChildren(this);"}
         cmd]
        [:div
          [:small
           "From " from]]
        (when-not (has-voted? req)
          [:a
           {:href (str "/vote?id=" from)}
           "Vote for this command"])])
     (:current-tally @app-state))])

(defn $submit-command []
  [:div
   ($section-header "Submit new command:")
   [:small
    {:style "max-width: 450px; display: block; text-align: justify;"}
    "Infinite loops would break it, so would removing vars we need to
     show the UI. Commands sent here _will_ be executed in the production
     repl if voted for, so try to not to submit shitty code"]
   [:form
     {:action "/submit-command"
      :method "post"}
     [:div
       [:textarea
        {:type "text"
         :name "command"
         :id "command"
         :placeholder "(+ 1 1)"}
        "(def hello \"world\")"]]
     [:button
      {:type "submit"}
      "Submit"]]])

;; This stylesheet gets printed for the main page within the <head> tag
(def stylesheet
  "body, html {
    margin: 0px;
    padding: 0px;
    font-family: sans-serif;
  }

  #wrapper {
    position: relative;
    padding: 40px;
  }")

;; Some links that are visible in the bottom of the page
(def handy-links
  [["My Initial Source" "/source"]
   ["GitHub" "https://github.com/victorb/livingthing"]
   ["Community Forum" "https://github.com/victorb/livingthing/discussions"]
   ["Current Vars" "/vars"]
   ["App-State" "/app-state"]
   ["Community ClojureDocs" "https://clojuredocs.org/clojure.core/def"]
   ["Clojure Reference" "https://clojure.org/api/cheatsheet"]])

(defn $links []
  [:div
    "Some handy links:"
    [:ul
      (map (fn [[text url]] [:li ($link text url)]) handy-links)]])

;; A little intro message to describe what this is all about
(defn $intro-message []
  [:div
   [:div "This is a once-in-a-lifetime REPL. Keep it alive, but do improve it. We're all in the same REPL."]
   [:div "How it works:"]
   [:ul
    [:li "Every hour, the command with most votes is selected and executed"]
    [:li
     "Maximum character size is dictated by the "
     [:code "max-size"]
     " var, currently set to "
     max-size]
    [:li "In the rest of the time, anyone can create new commands and vote for which command they want to execute"]
    [:li "You can only vote once per hour, per IP. IPs are kept in memory from process startup and are hashed with a random salt generated on process startup. "
     ($link "See my running source (hint: search for \"/source\")" "/source")]
    [:li "You can not undo your vote. Maybe this shall be the first attempted change?"]
    [:li "Server time should update once a second, if it doesn't, something went wrong"]]
   [:div "Server at launch supports:"]
   [:ul
    [:li "Running a HTTP server that serves HTML pages"]
    [:li "A HTML page showing: pending commands, executed commands, current votes and form for submitting new command"]
    [:li
     "Every hour (initially, dictated by round-duration-seconds, currently set to "
     round-duration-seconds
     " seconds) it evaluates the results and executes the command with the most votes"]]
   [:div
    ($links)]])

(defn $wrapper [req]
  [:div#wrapper
   [:h1
    "LivingThing.club"
    [:span
     {:style "color: grey; font-size: 14px;"}
     " aka \"Humanity Codes Clojure\""]]
   [:h2
    "Current server time: "
    (:current-time @app-state)
    " (round " (:current-round @app-state) ")"]
   [:div
    {:style "width: 50%; float: left;"}
    ($section-header "How LivingThing works")
    [:div
     {:style "font-family: serif; max-width: 700px;"}
     ($intro-message)]]
   [:div
     {:style "width: 50%; float: right;"}
     ($pending-vote-list req)
     ($submit-command)
     ($command-list)]])

;; Little handy script that connects to our WS endpoint and reloads the page
;; on any message. Messages get sent when app-state has relevant changes
(def js-reload-on-change
  "(new WebSocket('ws://'+window.location.host+'/ws')).addEventListener('message', () => window.location.reload());")

;; Wrapper that takes our hiccup elements and renders them to HTML
(defn http-index-page [req]
  {:body (hiccup/html
           (hiccup-page/html5
             [:head
              [:title "LivingThing.club"]
              [:style
               stylesheet]
              [:script
               js-reload-on-change]]
             [:body
              ($wrapper req)]))})

;; Adds a command to the list of pending commands. Rejects the addition if
;; either the size (characters) is too big, or if the user already submitted
;; a command this round.
(defn http-submit-command [req]
  (let [ip (:remote-addr req)
        id (hash-str ip)
        command (get-in req [:form-params "command"])]
    (if (> (count command) max-size)
      {:body (str "Too big! Max characters: " max-size ", yours was " (count command))}
      (if (get-in @app-state [:pending-commands id])
        {:body "You already submitted a command this round"}
        (do
          (swap! app-state assoc-in [:pending-commands id] command)
          {:status 302
           :headers {"Location" "/"}})))))

;; Performs a vote for a user for this current round
;; Checks if the vote is for/from it's owner and also if that IP already
;; voted before
(defn http-vote [req]
  (let [ip (:remote-addr req)
        id (hash-str ip)
        voted-for-id (get-in req [:params "id"])]
    (if (= id voted-for-id)
      {:body "You can't vote for yourself"}
      (if (get-in @app-state [:pending-votes id])
        {:body "You already voted this round"}
        (do
          (swap! app-state assoc-in [:pending-votes id] voted-for-id)
          @app-state
          {:status 302
           :headers {"Location" "/"}})))))

;; Shorthand for a ring-handler that outputs something via
;; with-out-str and pprint. Used for the /vars path
(defn pprint-handler [data]
  {:status 200
   :body (with-out-str (pprint data))})

;; Keeps track of current open WS channels
(defonce open-ws-channels (atom #{}))

;; Atom watcher that gets called every time app-state changes
;; Only checks for changes in :current-tally, so max updates are once per second
;; Sends `true` to the WS channel on every change
;; Clients react by reloading the page when this happens
(add-watch app-state :ws-watcher
  (fn [key atom old-state new-state]
    (when-not (= (:current-tally old-state)
                 (:current-tally new-state))
      (doseq [ws-channel @open-ws-channels]
        (httpkit/send! ws-channel (prn-str true))))))

;; Handler for WS connections, duh
(defn ws-handler [req]
  (httpkit/as-channel req
    {:on-close
     (fn [ch status]
       (println "on-close:" status)
       (swap! open-ws-channels disj ch))
     :on-open
     (fn [ch]
       (println "on-open:" ch)
       (swap! open-ws-channels conj ch))}))

;; This HTTP handler takes the file location of itself, loads the file
;; and adds syntax highlightning. Useful to be able to see the initial source
;; of the current runtime
(defn source-handler []
  (let [file-path (:file (meta #'source-handler))
        file-path (if (= (first file-path) \/)
                    file-path
                    (str "./src/" file-path))
        source-code (slurp file-path)]
    {:body (hiccup/html
             (hiccup-page/html5
               [:head
                [:style
                 "body, html {margin: 0; padding: 0; font-size: 18px;}
                  pre {margin: 0;}"
                 (generate-css)]]
               [:body
                (highlight-html source-code)]))}))

;; As generate-css and/or highlight-html is not the fastest of functions,
;; we trade memory usage for process usage, and cache the results of the first
;; call to source-handler indefinitly
;; First call: ~0,161 seconds, after that: ~0,018 seconds
(def memoized-source-handler (memoize source-handler))

;; routing function for our http server
(defn http-handler [req]
  (condp = (:uri req)
    "/" (http-index-page req)
    "/submit-command" (http-submit-command req)
    "/vote" (http-vote req)
    "/favicon.ico" {:status 404}
    "/ws" (ws-handler req)
    "/source" (memoized-source-handler)
    "/app-state" (pprint-handler @app-state)
    "/vars" (pprint-handler (vals (ns-publics 'livingthing.core)))))

;; Keep track of server-socket so we can close it if we want to.
(def server-socket (atom nil))

;; When deployed, we build this into a uberjar and execute it with java -jar
;; This is the function that gets called when running the jar
(defn -main [& args]
  (reset! server-socket
          (httpkit/run-server
            (-> #'http-handler
                (wrap-params))
            {:port 3825}))
  (start-server-ticker!))

;; Dev shorthands
(comment
  ;; Stops server
  (@server-socket)
  ;; Stops server ticker
  (stop-server-ticker!))

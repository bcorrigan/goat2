(ns org.goat.http.meals-server
  "Web diary for meals and URLs. Serves on port 21000."
  (:require [org.goat.db.meals :as db]
            [org.goat.db.urls :as urls-db]
            [org.goat.module.meals.photos :as photos]
            [org.httpkit.server :as server]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [hiccup2.core :refer [html raw]]
            [clojure.string :as str])
  (:import [java.time Instant ZoneId LocalDate]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; Helpers
;; ============================================================================

(def ^:private per-page 20)

(def ^:dynamic *base-path*
  "Prefix prepended to generated URLs for reverse-proxy setups.
   Set to the nginx location (e.g. \"/goat\") so links point to the proxied path.
   The nginx proxy_pass should use a trailing slash to strip this prefix
   before forwarding to the backend: proxy_pass http://127.0.0.1:21000/;"
  "/goat")

(defn- path
  "Prepend *base-path* to a relative URL."
  [s]
  (str *base-path* s))



(defn- fmt-date [epoch-ms]
  (let [fmt (DateTimeFormatter/ofPattern "d MMM yyyy")]
    (.format (-> (Instant/ofEpochMilli (long epoch-ms))
                 (.atZone (ZoneId/systemDefault))
                 .toLocalDate)
             fmt)))

(defn- fmt-date-secs [epoch-secs]
  (fmt-date (* (long epoch-secs) 1000)))

(defn- stars [score]
  (if (nil? score)
    (repeat 5 "☆")
    (let [n (-> score (max 0) (min 5))]
      (concat (repeat n "★") (repeat (- 5 n) "☆")))))

(defn- snippet [s max-len]
  (when s
    (let [s (str/trim s)]
      (if (> (count s) max-len)
        (str (subs s 0 max-len) "…")
        s))))

;; ============================================================================
;; CSS
;; ============================================================================

(def ^:private styles
  "
  * { margin:0; padding:0; box-sizing:border-box; }
  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
         background: #faf7f2; color: #2d2d2d; min-height: 100vh; }
  .container { max-width: 1100px; margin: 0 auto; padding: 20px; }
  header { background: #3d405b; color: #fff; padding: 20px 0; margin-bottom: 32px; }
  header .container { display: flex; justify-content: space-between; align-items: center;
                      flex-wrap: wrap; gap: 12px; }
  header h1 { font-size: 1.4rem; font-weight: 700; }
  header h1 a { color: #fff; text-decoration: none; }
  header nav { display: flex; gap: 4px; }
  header nav a { color: rgba(255,255,255,0.8); text-decoration: none; padding: 8px 16px;
                 border-radius: 6px; font-size: 0.95rem; font-weight: 500; transition: background 0.15s; }
  header nav a:hover, header nav a.active { background: rgba(255,255,255,0.15); color: #fff; }
  .search-bar { display: flex; gap: 8px; margin-bottom: 28px; }
  .search-bar input { flex: 1; padding: 10px 16px; border: 2px solid #e0d5c7; border-radius: 8px;
                      font-size: 1rem; background: #fff; }
  .search-bar input:focus { outline: none; border-color: #3d405b; }
  .search-bar button { padding: 10px 24px; background: #3d405b; color: #fff; border: none;
                       border-radius: 8px; font-size: 1rem; cursor: pointer; font-weight: 600; }
  .search-bar button:hover { background: #2d2f4a; }
  .meal-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
               gap: 20px; }
  .meal-card { background: #fff; border-radius: 12px; overflow: hidden;
               box-shadow: 0 2px 8px rgba(0,0,0,0.06); transition: transform 0.15s, box-shadow 0.15s;
               text-decoration: none; color: inherit; display: block; }
  .meal-card:hover { transform: translateY(-3px); box-shadow: 0 6px 20px rgba(0,0,0,0.10); }
  .meal-card-img { width: 100%; height: 180px; background: #f0ebe3;
                   display: flex; align-items: center; justify-content: center;
                   color: #c4b8a8; font-size: 3rem; overflow: hidden; }
  .meal-card-img img { width: 100%; height: 100%; object-fit: cover; }
  .meal-card-body { padding: 16px; }
  .meal-card-body h3 { font-size: 1.1rem; margin-bottom: 4px; color: #2d2d2d; }
  .stars { color: #e8a838; letter-spacing: 2px; font-size: 0.95rem; margin-bottom: 6px; display: block; }
  .meal-card-body .desc { font-size: 0.85rem; color: #6b6b6b; line-height: 1.4; margin-bottom: 8px; }
  .meal-card-meta { display: flex; align-items: center; gap: 8px; font-size: 0.8rem;
                    color: #8b7355; margin-top: 6px; }
  .ingredient-tag-sm { display: inline-block; background: #f0ebe3; color: #5c4a3a;
                       padding: 2px 8px; border-radius: 12px; margin: 0 4px 4px 0;
                       font-size: 0.75rem; }
  .ingredient-tag { display: inline-block; background: #f0ebe3; color: #5c4a3a;
                    padding: 4px 12px; border-radius: 20px; margin: 0 6px 6px 0;
                    font-size: 0.85rem; }
  .detail { background: #fff; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);
            overflow: hidden; }
  .detail-img { width: 100%; max-height: 500px; background: #f0ebe3; display: flex;
                align-items: center; justify-content: center; overflow: hidden; }
  .detail-img img { width: 100%; height: 100%; object-fit: contain; max-height: 500px; }
  .detail-no-img { width: 100%; height: 200px; background: #f0ebe3; display: flex;
                   align-items: center; justify-content: center; color: #c4b8a8; font-size: 4rem; }
  .detail-body { padding: 28px; }
  .detail-body h2 { font-size: 1.6rem; margin-bottom: 4px; }
  .detail-body > .stars { font-size: 1.2rem; margin-bottom: 16px; }
  .detail-section { margin-bottom: 20px; }
  .detail-section h4 { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 1px;
                       color: #8b7355; margin-bottom: 8px; }
  .detail-section p { line-height: 1.6; color: #4a4a4a; }
  .detail-meta { font-size: 0.85rem; color: #8b7355; margin-top: 24px; padding-top: 16px;
                 border-top: 1px solid #f0ebe3; }
  .back-link { display: inline-block; margin-bottom: 20px; color: #3d405b;
               text-decoration: none; font-weight: 600; }
  .back-link:hover { text-decoration: underline; }
  .pagination { display: flex; justify-content: center; gap: 16px; margin-top: 32px;
                align-items: center; }
  .pagination a, .pagination span { padding: 8px 16px; border-radius: 8px; font-weight: 600;
                                     text-decoration: none; }
  .pagination a { background: #3d405b; color: #fff; }
  .pagination a:hover { background: #2d2f4a; }
  .pagination span { color: #8b7355; }
  .empty-state { text-align: center; padding: 60px 20px; color: #8b7355; }
  .empty-state .icon { font-size: 4rem; margin-bottom: 16px; }
  .empty-state h2 { font-size: 1.4rem; margin-bottom: 8px; color: #5c4a3a; }
  .result-count { color: #8b7355; font-size: 0.9rem; margin-bottom: 20px; }
  .url-table { width: 100%; background: #fff; border-radius: 12px; overflow: hidden;
               box-shadow: 0 2px 8px rgba(0,0,0,0.06); border-collapse: collapse; }
  .url-table th { background: #3d405b; color: #fff; padding: 12px 16px; text-align: left;
                  font-weight: 600; font-size: 0.85rem; }
  .url-table td { padding: 10px 16px; border-bottom: 1px solid #f0ebe3; font-size: 0.9rem; }
  .url-table tr:hover td { background: #fdfcfa; }
  .url-table .url-col a { color: #3d405b; text-decoration: none; word-break: break-all; }
  .url-table .url-col a:hover { text-decoration: underline; }
  .url-table .msg-col { color: #6b6b6b; max-width: 400px; overflow: hidden;
                        text-overflow: ellipsis; white-space: nowrap; }
  footer { text-align: center; padding: 32px; color: #c4b8a8; font-size: 0.85rem;
           margin-top: 48px; }
  .add-meal-page { max-width: 600px; margin: 0 auto; }
  .form-card { background: #fff; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.06);
               padding: 32px; }
  .form-card h2 { font-size: 1.4rem; margin-bottom: 24px; color: #2d2d2d; }
  .form-error { background: #fef2f2; color: #dc2626; padding: 12px 16px; border-radius: 8px;
                margin-bottom: 20px; font-size: 0.9rem; }
  .form-group { margin-bottom: 20px; }
  .form-group label { display: block; font-size: 0.9rem; font-weight: 600; color: #5c4a3a;
                      margin-bottom: 6px; }
  .form-group input[type=\"text\"],
  .form-group input[type=\"file\"],
  .form-group textarea { width: 100%; padding: 10px 14px; border: 2px solid #e0d5c7;
                         border-radius: 8px; font-size: 1rem; font-family: inherit;
                         background: #fff; }
  .form-group input[type=\"text\"]:focus,
  .form-group textarea:focus { outline: none; border-color: #3d405b; }
  .form-group textarea { resize: vertical; }
  .form-group input[type=\"file\"] { padding: 8px; }
  .score-select { display: flex; gap: 4px; flex-wrap: wrap; }
  .score-option { cursor: pointer; }
  .score-option input { position: absolute; opacity: 0; width: 0; height: 0; }
  .score-option span { display: block; padding: 8px 10px; border-radius: 8px;
                       border: 2px solid #e0d5c7; font-size: 0.9rem; color: #e8a838;
                       transition: background 0.15s; }
  .score-option input:checked + span { background: #3d405b; border-color: #3d405b;
                                       color: #ffd700; }
  .score-option:hover span { background: #f0ebe3; }
  .form-actions { display: flex; gap: 12px; margin-top: 28px; }
  .form-actions button { padding: 12px 28px; background: #3d405b; color: #fff;
                         border: none; border-radius: 8px; font-size: 1rem; cursor: pointer;
                         font-weight: 600; }
  .form-actions button:hover { background: #2d2f4a; }
  .btn-cancel { padding: 12px 28px; border: 2px solid #e0d5c7; border-radius: 8px;
                color: #6b6b6b; text-decoration: none; font-weight: 600; font-size: 1rem; }
  .btn-cancel:hover { background: #f5f0ea; }
  .toolbar { display: flex; justify-content: space-between; align-items: center;
             margin-bottom: 20px; flex-wrap: wrap; gap: 12px; }
  .btn-add { display: inline-flex; align-items: center; gap: 6px; padding: 10px 20px;
             background: #4a7c59; color: #fff; border-radius: 8px; text-decoration: none;
             font-weight: 600; font-size: 0.95rem; }
  .btn-add:hover { background: #3a6347; }
  @media (max-width: 768px) {
    .container { padding: 12px; }
    header { padding: 14px 0; margin-bottom: 20px; }
    header h1 { font-size: 1.2rem; }
    header nav a { padding: 6px 12px; font-size: 0.85rem; }
    .meal-grid { grid-template-columns: 1fr; }
    .meal-card-img { height: 200px; }
    .search-bar { flex-direction: column; }
    .search-bar button { width: 100%; }
    .detail-body { padding: 16px; }
    .detail-body h2 { font-size: 1.3rem; }
    .detail-img { max-height: 300px; }
    .detail-img img { max-height: 300px; }
    .form-card { padding: 20px; }
    .form-card h2 { font-size: 1.2rem; }
    .form-actions { flex-direction: column; }
    .form-actions button, .btn-cancel { width: 100%; text-align: center; }
    .url-table { font-size: 0.8rem; display: block; overflow-x: auto; }
    .url-table .msg-col { max-width: 120px; }
    .pagination { flex-direction: column; align-items: center; gap: 8px; }
    .toolbar { flex-direction: column; align-items: stretch; }
    .btn-add { justify-content: center; }
    .score-select { gap: 2px; }
    .score-option span { padding: 6px 8px; font-size: 0.8rem; }
    footer { padding: 20px; font-size: 0.8rem; }
  }
  ")

;; ============================================================================
;; Layout
;; ============================================================================

(defn- layout
  "Base HTML wrapper for all pages."
  [title active-nav & body]
  (str "<!DOCTYPE html>\n"
       (html {:mode :html}
         [:html {:lang "en"}
          [:head
           [:meta {:charset "utf-8"}]
           [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
           [:title (str title " — House of the Future")]
           [:style (raw styles)]]
          [:body
           [:header
            [:div.container
             [:h1 [:a {:href (path "/")} "🏠 House of the Future"]]
             [:nav
              [:a {:href (path "/meals") :class (when (= active-nav :meals) "active")}
               "🍽️ Meals"]
              [:a {:href (path "/urls") :class (when (= active-nav :urls) "active")}
               "🔗 URLs"]]]]
           [:div.container
            body]
           [:footer "House of the Future"]]])))

;; ============================================================================
;; Shared components
;; ============================================================================

(defn- search-bar [q action]
  [:form.search-bar {:action (path action) :method "get"} 
   [:input {:type "text" :name "q" :placeholder "Search…" :value (or q "")}]
   [:button {:type "submit"} "Search"]])

(defn- page-qs
  "Build a full URL for a page number, preserving existing search param."
  [base-url page q]
  (str (path base-url) "?page=" page (when q (str "&q=" (java.net.URLEncoder/encode q "UTF-8")))))

(defn- pagination [base-url page total q]
  (let [pages (-> (/ total per-page) Math/ceil int)]
    (when (> pages 1)
      [:div.pagination
       (when (> page 1)
         [:a {:href (page-qs base-url (dec page) q)} "← Prev"])
       [:span (str "Page " page " of " pages)]
       (when (< page pages)
         [:a {:href (page-qs base-url (inc page) q)} "Next →"])])))

;; ============================================================================
;; Meals
;; ============================================================================

(defn- meal-card [meal]
  [:a {:class "meal-card" :href (path (str "/meal/" (:meal_id meal)))}
    [:div.meal-card-img
     (if (photos/photo-path (:meal_id meal))
       [:img {:src (path (str "/photo/" (:meal_id meal))) :alt (:title meal) :loading "lazy"}]
       "🍽️")]
    [:div.meal-card-body
     [:h3 (:title meal)]
     [:span.stars (apply str (stars (:score meal)))]
     (when (seq (:ingredients meal))
       [:div {:style "margin-bottom:6px"}
        (for [ing (:ingredients meal)]
           [:span.ingredient-tag-sm {:key ing} ing])])
     (when-let [d (snippet (:description meal) 120)]
       [:div.desc d])
     [:div.meal-card-meta
      (str "by " (:added_by meal))
      [:span "·"]
      (fmt-date (:added_date meal))]]])

(defn- meal-detail [meal]
  [:div
   [:a {:href (path "/meals") :class "back-link"} "← Back to meals"]
   [:div.detail
    (if (photos/photo-path (:meal_id meal))
      [:div.detail-img
       [:img {:src (path (str "/photo/" (:meal_id meal))) :alt (:title meal)}]]
      [:div.detail-no-img "🍽️"])
    [:div.detail-body
     [:h2 (:title meal)]
     [:span.stars (apply str (stars (:score meal)))]
     (when (seq (:ingredients meal))
       [:div.detail-section
        [:h4 "Ingredients"]
        (for [ing (:ingredients meal)]
          [:span.ingredient-tag {:key ing} ing])])
     (when (and (:description meal) (not (str/blank? (:description meal))))
       [:div.detail-section
        [:h4 "Notes"]
        [:p (:description meal)]])
      [:div.detail-meta
       (str "Added by " (:added_by meal)
            " on " (fmt-date (:added_date meal)))]
      [:div {:style "margin-top:24px"}
       [:form {:action (path (str "/meal/" (:meal_id meal) "/delete"))
               :method "post"
               :onsubmit "return confirm('Delete this meal?')"}
          [:button {:type "submit" :style "background:#dc2626;color:#fff;border:none;padding:8px 20px;border-radius:8px;cursor:pointer;font-weight:600"} "🗑 Delete Meal"]]]]]])

(defn- meals-index [meals page total q]
  (if (seq meals)
    [:div
     [:div.meal-grid
      (for [meal meals]
        (meal-card meal))]
     (pagination "/meals" page total q)]
    [:div.empty-state
     [:div.icon "🍽️"]
     [:h2 (if q "Nothing found" "No meals yet")]
     [:p (if q
           "Try a different search term."
           "Use the Telegram bot to add your first meal.")]]))

;; ============================================================================
;; Add Meal
;; ============================================================================

(defn- add-meal-form
  "The new-meal form. Accepts optional error message and preserved field values."
  [& {:keys [error title score ingredients description]}]
  [:div.add-meal-page
   [:a {:href (path "/meals") :class "back-link"} "← Back to meals"]
   [:div.form-card
    [:h2 "➕ Add a Meal"]
    (when error
      [:div.form-error error])
    [:form {:action (path "/meals/new") :method "post" :enctype "multipart/form-data"}
     [:div.form-group
      [:label "Photo"]
      [:input {:type "file" :name "photo" :accept "image/*"}]]
     [:div.form-group
      [:label "Title *"]
      [:input {:type "text" :name "title" :value (or title "")
               :required true :placeholder "e.g. Mac & Cheese"}]]
     [:div.form-group
      [:label "Score (1–5)"]
      [:div.score-select
       (for [n (range 1 6)]
         [:label.score-option
          [:input {:type "radio" :name "score" :value n
                   :checked (= n (some-> score parse-long))}]
          [:span (apply str (repeat n "★")) (apply str (repeat (- 5 n) "☆"))]])]]
     [:div.form-group
      [:label "Ingredients"]
      [:input {:type "text" :name "ingredients" :value (or ingredients "")
               :placeholder "Comma-separated, e.g. pasta, cheese, milk"}]]
     [:div.form-group
      [:label "Description"]
      [:textarea {:name "description" :rows 3
                  :placeholder "Optional notes…"}
       (or description "")]]
     [:div.form-actions
      [:button {:type "submit"} "💾 Save Meal"]
      [:a {:href (path "/meals") :class "btn-cancel"} "Cancel"]]]]])

;; ============================================================================
;; URLs
;; ============================================================================

(defn- url-row [u]
  [:tr
   [:td.url-col
    [:a {:href (:url u) :target "_blank" :rel "noopener"} (:url u)]]
   [:td.msg-col (snippet (:msg u) 200)]
   [:td (:sender u)]
   [:td (:chatname u)]
   [:td (fmt-date-secs (:time u))]])

(defn- urls-index [urls page total q]
  (if (seq urls)
    [:div
     [:table.url-table
      [:thead
       [:tr
        [:th "URL"]
        [:th "Message"]
        [:th "Sender"]
        [:th "Chat"]
        [:th "Date"]]]
      [:tbody
       (for [u urls]
         (url-row u))]]
     (pagination "/meals" page total q)]
    [:div.empty-state
     [:div.icon "🔗"]
     [:h2 (if q "Nothing found" "No URLs yet")]
     [:p (if q
           "Try a different search term."
           "URLs shared in chats will appear here.")]]))

;; ============================================================================
;; Helper: URL-aware page query
;; ============================================================================

(defn- normalize-page [page-str]
  (max 1 (or (some-> page-str parse-long) 1)))

;; ============================================================================
;; Multipart body parser
;; ============================================================================

(defn- byte-index-of
  "Find the index of byte pattern `needle` in `haystack`, starting at offset."
  [^bytes haystack ^bytes needle offset]
  (loop [i offset]
    (when (<= i (- (alength haystack) (alength needle)))
      (if (loop [j 0]
            (if (= j (alength needle))
              true
              (if (= (aget haystack (+ i j)) (aget needle j))
                (recur (inc j))
                false)))
        i
        (recur (inc i))))))

(defn- read-body-bytes
  "Read the full request body into a byte array."
  [body]
  (with-open [in (clojure.java.io/input-stream body)]
    (let [out (java.io.ByteArrayOutputStream.)
          buf (byte-array 4096)]
      (loop [n (.read in buf)]
        (when (pos? n)
          (.write out buf 0 n)
          (recur (.read in buf))))
      (.toByteArray out))))

(defn- parse-multipart
  "Parse multipart/form-data body bytes. Returns {field-name value} map."
  [body content-type]
  (when (and body content-type)
    (try
      (let [bs (read-body-bytes body)
            boundary (second (re-find #"boundary=([^\s;]+)" content-type))]
        (when boundary
          (let [delim (.getBytes (str "--" boundary) "UTF-8")
                delim-len (alength delim)
                ;; detect line ending style: \r\n or \n
                first-delim-end (byte-index-of bs delim (alength delim))
                header-sep (if (and first-delim-end
                                    (>= (alength bs) (+ first-delim-end delim-len 2))
                                    (= (aget bs (+ first-delim-end delim-len)) 13))
                            (.getBytes "\r\n\r\n" "UTF-8")
                            (.getBytes "\n\n" "UTF-8"))
                header-sep-len (alength header-sep)
                cr-after-sep? (= header-sep-len 4)  ;; \r\n\r\n = 4 bytes
                line-sep-len (if cr-after-sep? 2 1)]  ;; \r\n vs \n
            (loop [offset (alength delim)
                   params {}]
              (if (>= offset (- (alength bs) delim-len))
                params
                (if-let [header-end (byte-index-of bs header-sep offset)]
                  (let [header (String. bs offset (- header-end offset) "UTF-8")
                        body-start (+ header-end header-sep-len)]
                    (if-let [next-boundary (byte-index-of bs delim body-start)]
                      (let [field-name (second (re-find #"name=\"([^\"]+)\"" header))
                            filename   (second (re-find #"filename=\"([^\"]+)\"" header))
                            ;; trim trailing line ending before boundary
                            trim-before (if cr-after-sep? 1 0)
                            data-end   (- next-boundary trim-before)
                            next-offset (+ next-boundary delim-len line-sep-len)]
                        (if (and field-name filename)
                          (recur next-offset
                                 (assoc params field-name
                                        {:filename filename
                                         :content-type (or (second (re-find #"Content-Type:\s*(\S+)" header))
                                                          "application/octet-stream")
                                         :bytes (java.util.Arrays/copyOfRange bs body-start data-end)}))
                          (recur next-offset
                                 (assoc params field-name
                                        (String. bs body-start (- data-end body-start) "UTF-8")))))
                      params))
                  params))))))
      (catch Exception e
        (println "Multipart parse error:" (.getMessage e))
        nil))))

;; ============================================================================
;; Request parsing
;; ============================================================================

(defn- parse-request-body
  "Parse the request body into a params map, handling both
   multipart/form-data and application/x-www-form-urlencoded."
  [req]
  (let [content-type (get (:headers req) "content-type")]
    (if (and content-type (clojure.string/includes? content-type "multipart"))
      (parse-multipart (:body req) content-type)
      (when-let [b (:body req)]
        (try
          (let [body-str (slurp b)]
            (->> (clojure.string/split body-str #"&")
                 (map #(clojure.string/split % #"=" 2))
                 (map (fn [[k v]]
                        [(java.net.URLDecoder/decode (or k "") "UTF-8")
                         (java.net.URLDecoder/decode (or v "") "UTF-8")]))
                 (into {})))
          (catch Exception e
            (println "parse error:" (.getMessage e))
            nil))))))

;; ============================================================================
;; Routes
;; ============================================================================

(defroutes meals-routes
  ;; Home redirect
  (GET "/" []
    {:status 302 :headers {"Location" (path "/meals")} :body ""})

  ;; Meals
  (GET "/meals" [q page]
    (let [page (normalize-page page)
          limit per-page
          offset (* (dec page) limit)
          q (when (seq q) (str/trim q))]
      (if q
        (let [meals (db/search-meals-paged q limit offset)
              total (db/count-search-results q)]
          (layout "Meals" :meals
                (search-bar q "/meals")
                [:div.result-count
                 (str (if (pos? total) (str total " meal" (when (not= 1 total) "s")) "No meals")
                      " matching \"" q "\"")]
                (meals-index meals page total q)))
        (let [meals (db/list-meals :limit limit :offset offset)
              total (db/count-meals)]
          (layout "Meals" :meals
                [:div.toolbar
                 (search-bar nil "/meals")
                 [:a.btn-add {:href (path "/meals/new")} "➕ Add Meal"]]
                (meals-index meals page total nil))))))

  (GET "/meal/:id" [id]
    (if-let [meal-id (some-> id parse-long)]
      (if-let [meal (db/get-meal meal-id)]
        (layout (:title meal) :meals
              (meal-detail meal))
        (layout "Not Found" :meals
              [:div.empty-state
               [:div.icon "🤷"]
               [:h2 "Meal not found"]
               [:p "That meal doesn't exist or may have been deleted."]]))
      (layout "Not Found" :meals
            [:div.empty-state
             [:div.icon "🤷"]
              [:h2 "Meal not found"]]))))

  (POST "/meal/:id/delete" [id]
    (if-let [meal-id (some-> id parse-long)]
      (do
        (db/delete-meal meal-id)
        {:status 302
         :headers {"Location" (path "/meals")}
         :body ""})
      {:status 302
       :headers {"Location" (path "/meals")}
       :body ""}))

  (GET "/photo/:id" [id]
    (if-let [meal-id (some-> id parse-long)]
      (if-let [bs (photos/load-photo-bytes meal-id)]
        {:status 200
         :headers {"Content-Type" "image/jpeg"
                   "Cache-Control" "public, max-age=86400"}
         :body bs}
        {:status 404 :body "No photo"})
      {:status 400 :body "Invalid id"}))

  ;; Add meal form
  (GET "/meals/new" []
    (layout "Add Meal" :meals
          (add-meal-form))

  (POST "/meals/new" req
    (let [params (parse-request-body req)
          title  (some-> (get params "title") clojure.string/trim)
          score  (some-> (get params "score") parse-long)
          ings   (some-> (get params "ingredients") clojure.string/trim)
          desc   (some-> (get params "description") clojure.string/trim)
          photo  (get params "photo")]
      (if (clojure.string/blank? title)
        (layout "Add Meal" :meals
              (add-meal-form
                :error "Title is required."
                :title title :score (str score)
                :ingredients ings :description desc))
        (try
          (let [id (db/add-meal {:title title
                                 :score score
                                 :description (when (seq desc) desc)
                                 :added-by "web"
                                 :chat-id 0})]
            (db/add-ingredients id (when ings (clojure.string/split ings #",")))
            (when (and photo (map? photo) (:bytes photo))
              (try
                (photos/save-photo! id (:bytes photo))
                (catch Exception e
                  (println "Photo upload error:" (.getMessage e)))))
            {:status 302
             :headers {"Location" (path (str "/meal/" id))}
             :body ""})
          (catch Exception e
            (println "DB error:" (.getMessage e))
            (layout "Add Meal" :meals
                  (add-meal-form
                    :error (str "Database error: " (.getMessage e))
                    :title title :score (str score)
                    :ingredients ings :description desc)))))))

  ;; URLs
  (GET "/urls" [q page]
    (let [page (normalize-page page)
          limit per-page
          offset (* (dec page) limit)
          q (when (seq q) (str/trim q))
          search (if q (str "%" q "%") "%")
          urls (urls-db/get-urls limit (str offset) search)
          ;; Count query
          all-urls (urls-db/get-urls 1000 0 search)
          total (count all-urls)]
      (if q
        (layout "URLs" :urls
              (search-bar q "/urls")
              [:div.result-count
               (str (if (pos? total) (str total " URL" (when (not= 1 total) "s")) "No URLs")
                    " matching \"" q "\"")]
               (urls-index urls page total q))
        (layout "URLs" :urls
              (search-bar nil "/urls")
              (urls-index urls page total nil)))))

  (route/not-found
    (layout "Not Found" :meals
          [:div.empty-state
           [:div.icon "🤷"]
           [:h2 "Page not found"]
            [:p "Nothing here."]])))

;; ============================================================================
;; Server lifecycle
;; ============================================================================

(defonce ^{:doc "The running server instance."}
  server (atom nil))



(defn start!
  "Start the unified web server on the given port (default 21000).
   Accepts optional base-path for reverse proxy (e.g. \"/goat\")."
  ([] (start! 21000 "/goat"))
  ([port] (start! port "/goat"))
  ([port base-path]
   (when-not @server
     (alter-var-root #'*base-path* (constantly (or base-path "")))
     (reset! server (server/run-server meals-routes
                                       {:port port :legacy-return-value? false}))
     (println (str "🏠 House of the Future web server started on http://localhost:" port
                   (when (seq *base-path*) (str " (base: " *base-path* ")")))))))

(defn stop!
  "Stop the web server."
  []
  (when-let [s @server]
    (server/server-stop! s)
    (reset! server nil)
    (println "House of the Future web server stopped")))

;; ============================================================================
;; REPL helpers
;; ============================================================================

(comment
  (start! 21000)
  (stop!)

  (db/list-meals :limit 5 :offset 0)
  (urls-db/get-urls 10 0 "%")
  )

(require 'yaml)

(defun xtv--imdb-score-norm (score)
  "Turn a string (SCORE) from n/10 (e.g. 5.5) into
n/100 (e.g. 55). Returns a number"
  (cond ((eq score nil) nil)
        ((string-blank-p score) nil)
        ((string= (upcase score) "N/A") nil)
        (t (floor (* 10 (string-to-number score))))))

(defun xtv--omdb-adaptor (result)
  (let* ((ht (make-hash-table)))
    (puthash 'title (cdr (assoc 'Title result)) ht)
    (puthash 'year (cdr (assoc 'Year result)) ht)
    (puthash 'maturity (cdr (assoc 'Rated result)) ht)
    (puthash 'genres (xstr-split "," (cdr (assoc 'Genre result)) t) ht)
    (puthash 'directors (xstr-split "," (cdr (assoc 'Director result))) ht)
    (puthash 'writers (xstr-split "," (cdr (assoc 'Writer result))) ht)
    (puthash 'actors (xstr-split "," (cdr (assoc 'Actors result))) ht)
    (puthash 'plot (cdr (assoc 'Plot result)) ht)
    (puthash 'cover (cdr (assoc 'Poster result)) ht)
    (puthash 'imdb-id (cdr (assoc 'imdbID result)) ht)
    (puthash 'category (cdr (assoc 'Type result)) ht)
    (puthash 'metascore (cdr (assoc 'Metascore result)) ht)
    (puthash 'imdb-rating (xtv--imdb-score-norm (cdr (assoc 'imdbRating result))) ht)
    ht))

(cl-defun xtv--omdb-ok (data)
  (string= "True" (cdr (assoc 'Response data))))

;; don't use xcheck here; issues with loading libraries early in the boot
(when (xnil-ish 'xtv-omdb-key)
  (error "You must set xtv-omdb-key" ))

(cl-defun xtv--omdb-get (param)
  (let ((result nil))
    (request "http://www.omdbapi.com"
      :params `(("apikey" . ,xtv-omdb-key) ,param ("v" . 1))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (setq result (request-response-data response)))))
    (if (and (not (eq nil result)) (xtv--omdb-ok result))
        (xtv--omdb-adaptor result)
      result)))

(defun xtv-get-by-name (name) (xtv--omdb-get `("t" . ,name)))
(defun xtv-get-by-id (id) (xtv--omdb-get `("i" . ,id)))

(defun xtv-prompt (title)
  "Giving a TITLE, prompt the user to accept the
data. If the user does not accept it, then it will
ask for the IMDb ID.

Returns nil or a TV hash table."
  (let* ((from-name (xtv-get-by-name title))
         (result nil)
         (from-id nil)
         (imdb-id nil))
    (if (smenu-confirm-data from-name)
        from-name
      (setq imdb-id (xprompt "IMDb ID"))
      (when imdb-id (setq from-id (xtv-get-by-id imdb-id)))
      (when (and from-id (smenu-confirm-data from-id))
        (setq result from-id))
      result)))

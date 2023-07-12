(defun xurl-http? (url)
  (and
   (org-url-p url)
   (not (eq nil (string-match-p "^https?:\/\/" url)))))

(defun xurl-https? (url)
  (and
   (org-url-p url)
   (not (eq nil (string-match-p "^https:\/\/" url)))))

(defun xurl-simg (url)
  "Validate that URL is HTTPS and has an image
extension. Returns URL. Raises error if it's not."
  (xcheck (cons "URL must be HTTPS with an image extension" url)
          (org-file-image-p url)
          (xurl-https? url))
  url)

;; TODO: this could do with some improvement: i.e. matches a video link, becuase that's all
;; that it's ever used for
(setq xurl-yt-re "^https://\\(music.\\|www.\\)?\\(youtube.com\\|youtu.be\\)\\(\/\\|\/.+\\)?$")
(defun xurl-yt? (url)
  (not (eq nil (string-match-p xurl-yt-re url))))

(defun xurl-imdb (id)
  "Given an ID, return a full IMDb link."
  (format "https://www.imdb.com/title/%s/" id))

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

(setq xurl-yt-re "^https://\\(music.\\|www.\\)?\\(youtube.com\\|youtu.be\\)\\(\/\\|\/.+\\)?$")
(defun xurl-yt? (url)
  (not (eq nil (string-match-p xurl-yt-re url))))

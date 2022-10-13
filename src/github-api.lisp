(in-package :github)

(defun get-pr (pr-code gh-token)
  "Get PR object by hash"
  (let ((url (format nil "https://api.github.com/repos/kyma-project/kyma/pulls/~a" pr-code))
        (headers (list (cons 'authorization gh-token)
                       (cons 'accept "application/vnd.github+json"))))
    (jonathan:parse (dex:get url :headers headers))))

(defun merged? (pr)
  "Check if PR (in the form of plist) was merged"
  (getf pr :|merged|))

(defun pr->merge-sha (pr)
  "Extract merge hash from pr (in the form of plist)"
  (if (merged? pr)
      (getf pr :|merge_commit_sha|)
      nil))

(defun merge-hash (pr token &optional length)
  "Returns merge hash of PR by number. Can be truncated."
  (let* ((pr (get-pr pr token))
         (sha (pr->merge-sha pr)))
    (if (and sha (numberp length))
        (str:shorten length sha :ellipsis "")
        sha)))

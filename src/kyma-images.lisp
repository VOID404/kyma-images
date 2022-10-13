(in-package :kyma-images)

;; Define your project functionality here...

(defun get-yamls (&optional (path #P"./"))
  (let ((*default-pathname-defaults* path))
    (append
     (directory #P"**/values.yaml")
     (directory #P"**/values.yml"))))

(defvar image-pred (ppcre:create-scanner "\\s+version:[\\W]*(PR-(\\w+)).+$"))

(defun replace-lines (file replacer &optional (tmp-postfix ".tmp"))
  (let ((renamed-file (rename-file file (format nil "~a~a" file tmp-postfix))))
    (with-open-file (fi renamed-file :direction :input)
      (with-open-file (fo file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
        (loop :for line = (read-line fi nil 'eof)
              :until (eq line 'eof)
              :for new-line = (or (funcall replacer line) line)
              :do (write-line new-line fo))))))


(defun gh-replacer (gh-token)
    (lambda (line)
      (ppcre:register-groups-bind (orig pr) (image-pred line)
        (let* ((new-img (merge-hash pr gh-token 8))
               (new-line (ppcre:regex-replace orig line new-img)))
          (format t "~areplacing ~a -> ~a~%" #\tab orig new-img)
          new-line))))


(defun help ()
  (format t "~&Usage:

  kyma-images [path-to-kyma]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (let* ((path (uiop:parse-native-namestring (or (first argv)
                                                 "./")))
         (ymls (get-yamls path))
         (token (sb-unix::posix-getenv "GH_TOKEN"))
         (replacer (gh-replacer token)))
    (loop :for file :in ymls
          :do
             (format t "Processing ~a:~%" file)
             (replace-lines file replacer)
             (format t "---~%"))))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))

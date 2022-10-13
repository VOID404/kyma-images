(defpackage :github
  (:use :cl)
  (:export :merge-hash
           :get-pr))

(defpackage :kyma-images
  (:use :cl :github)
  (:export :main))

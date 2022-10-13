(in-package :asdf-user)
(defsystem "kyma-images-tests"
  :description "Test suite for the kyma-images system"
  :author "VOID404 <wojciech.nawa@gmail.com>"
  :version "0.0.1"
  :depends-on (:kyma-images
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-kyma-images"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )

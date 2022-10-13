
(load "kyma-images.asd")
(load "kyma-images-tests.asd")

(ql:quickload "kyma-images-tests")

(in-package :kyma-images-tests)

(uiop:quit (if (run-all-tests) 0 1))

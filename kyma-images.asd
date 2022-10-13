(in-package :asdf-user)

(defsystem "kyma-images"
  :author "VOID404 <wojciech.nawa@gmail.com>"
  :version "0.0.1"
  :license "MIT"
  :description "App for bumping kyma images before release"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (:dexador
               :jonathan
               :str
               :cl-ppcre)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "github-api")
                                     (:file "kyma-images"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "kyma-images"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "kyma-images:main")

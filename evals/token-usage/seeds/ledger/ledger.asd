(defsystem "ledger"
  :version "0.1.0"
  :description "A small ledger: accounts, transfers, monthly reports"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "money")
               (:file "accounts")
               (:file "reports"))
  :in-order-to ((test-op (test-op "ledger/tests"))))

(defsystem "ledger/tests"
  :depends-on ("ledger" "rove")
  :pathname "tests"
  :components ((:file "ledger-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

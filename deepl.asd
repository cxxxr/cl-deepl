(defsystem "deepl"
  :class :package-inferred-system
  :depends-on ("deepl/deepl")
  :in-order-to ((test-op (test-op "deepl/test"))))

(defsystem "deepl/test"
  :class :package-inferred-system
  :depends-on ("deepl/test")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))

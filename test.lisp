(defpackage :deepl/test
  (:use :cl :rove)
  (:import-from :deepl))
(in-package :deepl/test)

(defparameter +languages+ "
[
  {
    \"language\": \"DE\",
    \"name\": \"German\"
  },
  {
    \"language\": \"EN\",
    \"name\": \"English\"
  },
  {
    \"language\": \"ES\",
    \"name\": \"Spanish\"
  },
  {
    \"language\": \"FR\",
    \"name\": \"French\"
  },
  {
    \"language\": \"IT\",
    \"name\": \"Italian\"
  },
  {
    \"language\": \"JA\",
    \"name\": \"Japanese\"
  },
  {
    \"language\": \"NL\",
    \"name\": \"Dutch\"
  },
  {
    \"language\": \"PL\",
    \"name\": \"Polish\"
  },
  {
    \"language\": \"PT\",
    \"name\": \"Portuguese\"
  },
  {
    \"language\": \"RU\",
    \"name\": \"Russian\"
  },
  {
    \"language\": \"ZH\",
    \"name\": \"Chinese\"
  }
]")

(defun required-argument-p (condition key)
  (and (typep condition 'error)
       (equal (princ-to-string condition)
              (format nil "Required argument ~S missing." key))))

(defun type-error-p (condition datum expected-type)
  (and (typep condition 'type-error)
       (equal (type-error-datum condition) datum)
       (equal (type-error-expected-type condition) expected-type)))

(defmacro catch-error (form)
  `(nth-value 1 (ignore-errors ,form)))

(deftest translate-params
  (testing "required argument"
    (ok (required-argument-p (catch-error (make-instance 'deepl::translate-params))
                             :text))
    (ok (required-argument-p (catch-error (make-instance 'deepl::translate-params :text "hello"))
                             :target-lang))
    (ok (typep (make-instance 'deepl::translate-params :text "hello" :target-lang :ja)
               'deepl::translate-params)))
  (testing "check type"
    (testing "text"
      (ok (typep (make-instance 'deepl::translate-params :text "hello" :target-lang :ja)
                 'deepl::translate-params))
      (ok (typep (make-instance 'deepl::translate-params :text '("hello" "world") :target-lang :ja)
                 'deepl::translate-params))
      (ok (type-error-p (catch-error
                         (make-instance 'deepl::translate-params
                                        :text 1
                                        :target-lang :ja))
                        1
                        '(or string deepl::string-list))))
    (testing "source-lang"
      (dolist (source-lang '(:DE :EN :FR :IT :JA :ES :NL :PL :PT :RU :ZH))
        (ok (typep (make-instance 'deepl::translate-params
                                  :text "hello"
                                  :target-lang :ja
                                  :source-lang source-lang)
                   'deepl::translate-params)))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :ja
                                                    :source-lang :xxx))
                        :xxx
                        'deepl::source-lang))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :ja
                                                    :source-lang "de"))
                        "de"
                        'deepl::source-lang)))
    (testing "target-lang"
      (dolist (target-lang '(:DE :EN-GB :EN-US :EN :FR :IT :JA :ES :NL :PL :PT-PT :PT-BR :PT :RU :ZH))
        (ok (typep (make-instance 'deepl::translate-params
                                  :text "hello"
                                  :target-lang target-lang
                                  :source-lang :de)
                   'deepl::translate-params)))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :de
                                                    :source-lang :xxx))
                        :xxx
                        'deepl::source-lang))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :de
                                                    :source-lang "de"))
                        "de"
                        'deepl::source-lang)))
    (testing "split-sentences"
      (dolist (split-sentences '(:0 :1 :nonewlines))
        (ok (typep (make-instance 'deepl::translate-params
                                  :text "hello"
                                  :target-lang :ja
                                  :source-lang :en
                                  :split-sentences split-sentences)
                   'deepl::translate-params)))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :ja
                                                    :source-lang :en
                                                    :split-sentences 0))
                        0
                        'deepl::split-sentences)))
    (testing "preserve-formatting"
      (dolist (preserve-formatting '(:0 :1))
        (ok (typep (make-instance 'deepl::translate-params
                                  :text "hello"
                                  :target-lang :ja
                                  :source-lang :en
                                  :preserve-formatting preserve-formatting)
                   'deepl::translate-params)))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :ja
                                                    :source-lang :en
                                                    :preserve-formatting 0))
                        0
                        'deepl::preserve-formatting)))
    (testing "formality"
      (dolist (formality '(:default :more :less))
        (ok (typep (make-instance 'deepl::translate-params
                                  :text "hello"
                                  :target-lang :ja
                                  :source-lang :en
                                  :formality formality)
                   'deepl::translate-params)))
      (ok (type-error-p (catch-error (make-instance 'deepl::translate-params
                                                    :text "hello"
                                                    :target-lang :ja
                                                    :source-lang :en
                                                    :formality "default"))
                        "default"
                        'deepl::formality))))
  (testing "params-to-alist"
    (ok (equal '(("text" . "hello")
                 ("target_lang" . "JA"))
               (deepl::params-to-alist (make-instance 'deepl::translate-params
                                                      :text "hello"
                                                      :target-lang :ja))))
    (ok (equal '(("text" . "hello")
                 ("text" . "world")
                 ("target_lang" . "JA"))
               (deepl::params-to-alist (make-instance 'deepl::translate-params
                                                      :text '("hello" "world")
                                                      :target-lang :ja))))
    (ok (equal '(("text" . "hello")
                 ("text" . "world")
                 ("target_lang" . "JA")
                 ("preserve_formatting" . "1")
                 ("formality" . "more"))
               (deepl::params-to-alist (make-instance 'deepl::translate-params
                                                      :text '("hello" "world")
                                                      :target-lang :ja
                                                      :preserve-formatting :1
                                                      :formality :more))))))

(defclass test-client (deepl:client)
  ((json :initarg :json
         :reader test-client-json))
  (:default-initargs :auth-key "..."))

(defmethod deepl::post-request ((client test-client) (path-keyword (eql :translate)) params)
  (test-client-json client))

(defmethod deepl::post-request ((client test-client) (path-keyword (eql :languages)) params)
  +languages+)

(defmethod deepl::post-request ((client test-client) (path-keyword (eql :usage)) params)
  (st-json:write-json-to-string
   (st-json:jso "character_count" 300
                "character_limit" 1000000)))

(deftest translate
  (let* ((client (make-instance 'test-client
                                :json (st-json:write-json-to-string
                                       (st-json:jso "translations"
                                                    (list (st-json:jso "detected_source_language" "EN"
                                                                       "text" "こんにちわ"))))))
         (result (deepl:translate client "hello" :target-lang :ja)))
    (ok (alexandria:set-equal
         (mapcar (lambda (translation)
                   (list :detected-source-language (deepl:translation-detected-source-language translation)
                         :text (deepl:translation-text translation)))
                 result)
         '((:detected-source-language :en :text "こんにちわ"))
         :test #'equal))))

(deftest languages
  (let ((result (deepl:languages (make-instance 'test-client))))
    (ok (alexandria:set-equal
         (mapcar (lambda (language)
                   (list :keyword (deepl:language-keyword language)
                         :name (deepl:language-name language)))
                 result)
         '((:KEYWORD :DE :NAME "German")
           (:KEYWORD :EN :NAME "English")
           (:KEYWORD :ES :NAME "Spanish")
           (:KEYWORD :FR :NAME "French")
           (:KEYWORD :IT :NAME "Italian")
           (:KEYWORD :JA :NAME "Japanese")
           (:KEYWORD :NL :NAME "Dutch")
           (:KEYWORD :PL :NAME "Polish")
           (:KEYWORD :PT :NAME "Portuguese")
           (:KEYWORD :RU :NAME "Russian")
           (:KEYWORD :ZH :NAME "Chinese"))
         :test #'equal))))

(deftest usage
  (let ((result (deepl:usage (make-instance 'test-client))))
    (ok (typep result 'deepl:usage))
    (ok (= (deepl:usage-character-count result) 300))
    (ok (= (deepl:usage-character-limit result) 1000000))))

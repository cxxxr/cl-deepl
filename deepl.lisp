(defpackage :deepl/deepl
  (:nicknames :deepl)
  (:use :cl :alexandria :deepl/utils)
  (:import-from :closer-mop)
  (:import-from :babel)
  (:import-from :dexador)
  (:import-from :st-json)
  (:import-from :cl-change-case)
  (:export :deepl-error
           :deepl-request-error
           :client
           :translation
           :translation-text
           :translation-detected-source-language
           :translate
           :usage
           :usage-character-count
           :usage-character-limit
           :language
           :language-keyword
           :language-name
           :languages))
(in-package :deepl/deepl)

#+sbcl(sb-ext:lock-package *package*)

(define-condition deepl-error (error)
  ())

(define-condition deepl-request-error (deepl-error)
  ((status-code :initarg :status-code)
   (json :initarg :json))
  (:report (lambda (c s)
             (with-slots (status-code json) c
               (format s "code: ~A" status-code)
               (when (plusp (length json))
                 (format s ", body: ~A" json))))))


(defgeneric params-to-alist (params))

(defclass request-params ()
  ())

(defmethod initialize-instance :after ((request-params request-params) &key &allow-other-keys)
  (map-bound-slots (lambda (slot-definition value)
                     (let ((type (closer-mop:slot-definition-type slot-definition)))
                       (unless (typep value type)
                         (error 'type-error :expected-type type :datum value))))
                   request-params))


(defclass client ()
  ((auth-key
    :initform (required-argument :auth-key)
    :initarg :auth-key
    :reader client-auth-key)
   (base-uri
    :initform "https://api.deepl.com/v2"
    :reader client-base-uri)
   (verbose
    :initarg :verbose
    :initform nil
    :reader client-verbose-p)))

(defmethod post-request ((client client) path-keyword params)
  (handler-case
      (dex:post (format nil "~A/~A" (client-base-uri client) (string-downcase path-keyword))
                :content `(("auth_key" . ,(client-auth-key client))
                           ,@(when params (params-to-alist params)))
                :verbose (client-verbose-p client))
    (dex:http-request-failed (c)
      (error 'deepl-request-error
             :status-code (dex:response-status c)
             :json (babel:octets-to-string (dex:response-body c))))))


(deftype string-list ()
  '(and list (satisfies string-list-p)))

(deftype source-lang ()
  '(member :DE :EN :FR :IT :JA :ES :NL :PL :PT :RU :ZH))

(deftype target-lang ()
  '(member :DE :EN-GB :EN-US :EN :FR :IT :JA :ES :NL :PL :PT-PT :PT-BR :PT :RU :ZH))

(deftype split-sentences ()
  '(member :0 :1 :nonewlines))

(deftype preserve-formatting ()
  '(member :0 :1))

(deftype formality ()
  '(member :default :more :less))

(defstruct translation
  detected-source-language
  text)

(defclass translate-params (request-params)
  ((text
    :initarg :text
    :initform (required-argument :text)
    :type (or string string-list))
   (source-lang
    :initarg :source-lang
    :type source-lang)
   (target-lang
    :initarg :target-lang
    :initform (required-argument :target-lang)
    :type target-lang)
   (split-sentences
    :initarg :split-sentences
    :type split-sentences)
   (preserve-formatting
    :initarg :preserve-formatting
    :type preserve-formatting)
   (formality
    :initarg :formality
    :type formality)
   ;; xml
   (tag-handling
    :initarg :tag-handling)
   (non-splitting-tags
    :initarg :non-splitting-tags)
   (outline-detection
    :initarg :outline-detection)
   (ignore-tags
    :initarg :ignore-tags)))

(defmethod params-to-alist ((params translate-params))
  (let ((alist '()))
    (flet ((add (slot-name value)
             (push (cons (cl-change-case:snake-case (string slot-name))
                         value)
                   alist)))
      (map-bound-slots (lambda (slot-definition slot-value)
                         (let ((slot-name (closer-mop:slot-definition-name slot-definition)))
                           (case slot-name
                             (text
                              (dolist (text (uiop:ensure-list slot-value))
                                (add slot-name text)))
                             ((source-lang target-lang split-sentences preserve-formatting)
                              (add slot-name (string-upcase slot-value)))
                             ((formality)
                              (add slot-name (string-downcase slot-value)))
                             (otherwise
                              (add slot-name slot-value)))))
                       params)
      (nreverse alist))))

(defun parse-translate-response (body)
  (let ((jso (st-json:read-json body)))
    (loop :for translation :in (st-json:getjso "translations" jso)
          :collect (make-translation
                    :detected-source-language (make-keyword
                                               (st-json:getjso "detected_source_language"
                                                               translation))
                    :text (st-json:getjso "text" translation)))))

(defun translate (client text &rest args &key target-lang source-lang split-sentences preserve-formatting formality)
  (declare (ignore target-lang source-lang split-sentences preserve-formatting formality))
  (parse-translate-response (post-request client
                                          :translate
                                          (apply #'make-instance
                                                 'translate-params
                                                 :text text
                                                 args))))


(defstruct usage
  character-count
  character-limit)

(defun parse-usage-response (body)
  (let ((jso (st-json:read-json body)))
    (make-usage :character-count (st-json:getjso "character_count" jso)
                :character-limit (st-json:getjso "character_limit" jso))))

(defun usage (client)
  (parse-usage-response (post-request client :usage nil)))


(defstruct language
  keyword
  name)

(defun parse-languages-response (body)
  (loop :for jso :in (st-json:read-json body)
        :collect (make-language :keyword (make-keyword (st-json:getjso "language" jso))
                                :name (st-json:getjso "name" jso))))

(defun languages (client)
  (parse-languages-response (post-request client :languages nil)))

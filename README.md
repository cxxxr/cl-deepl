# cl-deepl

## Usage

```common-lisp
(defvar *client* (make-instance 'deepl:client :auth-key "..."))

(deepl:translate *client* "hello" :target-lang :ja)
=> (#S(DEEPL:TRANSLATION :DETECTED-SOURCE-LANGUAGE :EN :TEXT "こんにちわ"))

(deepl:translate *client* '("hello" "world") :target-lang :ja)
=> (#S(DEEPL:TRANSLATION :DETECTED-SOURCE-LANGUAGE :EN :TEXT "こんにちわ")
    #S(DEEPL:TRANSLATION :DETECTED-SOURCE-LANGUAGE :EN :TEXT "せかい"))

(deepl:usage *client*)
=> #S(DEEPL:USAGE :CHARACTER-COUNT 318 :CHARACTER-LIMIT 1000000)

(deepl:languages *client*)
=> (#S(DEEPL:LANGUAGE :KEYWORD :DE :NAME "German")
    #S(DEEPL:LANGUAGE :KEYWORD :EN :NAME "English")
    #S(DEEPL:LANGUAGE :KEYWORD :ES :NAME "Spanish")
    #S(DEEPL:LANGUAGE :KEYWORD :FR :NAME "French")
    #S(DEEPL:LANGUAGE :KEYWORD :IT :NAME "Italian")
    #S(DEEPL:LANGUAGE :KEYWORD :JA :NAME "Japanese")
    #S(DEEPL:LANGUAGE :KEYWORD :NL :NAME "Dutch")
    #S(DEEPL:LANGUAGE :KEYWORD :PL :NAME "Polish")
    #S(DEEPL:LANGUAGE :KEYWORD :PT :NAME "Portuguese")
    #S(DEEPL:LANGUAGE :KEYWORD :RU :NAME "Russian")
    #S(DEEPL:LANGUAGE :KEYWORD :ZH :NAME "Chinese"))
```

## License
MIT

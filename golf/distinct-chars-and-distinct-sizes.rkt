#lang racket
; https://codegolf.stackexchange.com/questions/277441/distinct-characters-and-distinct-sizes?answertab=modifieddesc#tab-top

(define solve
(λ(s'set-count,list->set)[apply +',s{set->list,(map length [group-by(λ(x)x){sort s <}])}])
)

(define tests
  (hash
   "" 0
   "a" 2
   "aa" 3
   "aaa" 4
   "ab" 3
   "aab" 5
   "aaab" 6
   "abab" 4
   "abc" 4
   "abccbcb" 7
   "abaccac" 7
   "abcaaccac" 8
   "emmmmmmmm" 11
   "abcc" 6
   "aabcc" 6
   "aabbcc" 5))

(define ez-solve (compose solve bytes->list string->bytes/utf-8))

(require rackunit)
(for ([(in out) tests])
  (check-equal? (ez-solve in) out in))

# Neon Distillation
Neon 가스는 liquid air를 fractional distillation으로 생산한다.
이 project에서는 Neon language의 syntax를 실험한다.

참고
* [Neon - Wikipedia](https://en.wikipedia.org/wiki/Neon)

## 다른 언어 조사

### Go

* [The Go Programming Language Specification](https://golang.org/ref/spec)

* `type`, `func`등 처음에 무엇인지를 적도록 해서 parsing을 쉽게 함.

* Rune: \uXXXX, \UXXXXXXXX

* string: immutable

* slice: array를 storage로 사용
  - immutable한 string의 일부 data를 slice로 끊어내는 방식으로 text 기반의 HTTP protocol parsing을 효율적으로 할 수 있다.

* iota: constant generator

* class밖에 method를 선언해서, 확장이 쉬움
  - receiver는 함수의 첫번째 parameter임

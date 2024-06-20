picolens
========
[![CI tests](https://github.com/shamazmazum/picolens/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/picolens/actions/workflows/test.yml)

A tiny library for working with lenses:

``` lisp
CL-USER> (picolens:set #'picolens:caar 10 '((1 . 2) 3))
((10 . 2) 3)
CL-USER> (picolens:view #'picolens:caar '((1 . 2) 3))
1
```

For more info read the [documentation](https://shamazmazum.github.io/picolens/).

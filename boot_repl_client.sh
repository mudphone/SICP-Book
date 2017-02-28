#!/bin/bash
# https://github.com/adzerk-oss/boot-cljs-repl
# (hoplon.app_pages._chapter1_1_DOT_html/square 2) => 4
# use (print (.-stack (js/Error. "hello"))) to find the generated ns
#
# use (start-repl) to connect the repl to the browser
boot repl --client

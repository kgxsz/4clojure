(set-env!
  :source-paths #{"src" "test"}
  :dependencies '[[adzerk/boot-test "1.0.4" :scope "test"]])

(require '[adzerk.boot-test :refer :all])

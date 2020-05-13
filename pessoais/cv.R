cv = function(x, ...) {
  sd(x, ...) / mean(x,...) * 100
}
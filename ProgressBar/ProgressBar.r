ProgressBar <- setRefClass(
  "ProgressBar",
  fields = list(
    total   = "numeric",
    step    = "numeric",
    iter    = "numeric",
    style   = "character",
    counter = "logical"
  ),
  methods = list(
    initialize = function(total, step=5, style="default", counter=TRUE) {
      total   <<- total
      step    <<- step
      style   <<- style
      counter <<- counter
      iter    <<- 0
    },
    run = function() {
      displayBar <- function(completed, todo) {
        cat("\r[", rep("#", completed), rep("-", todo), "]", sep="")
      }
      displayCounter <- function(count) {
        cat("", count, "%")
      }
      
      iter      <<- iter + 1
      count     <- round(iter / total * 100)
      completed <- count %/% step
      todo      <- (100 - count + count %% step) %/% step
      displayBar(completed, todo)
      
      if(counter)
        displayCounter(count)
    }
  )
)
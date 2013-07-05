Vector <- setRefClass(
  "Vector",
  fields = list(
    values = "vector",
    length = "numeric"
  ),
  methods = list(
    initialize = function() {
      values <<- vector()
      length <<- 0
    },
    append = function(value) {
      values <<- c(values, value)
      length <<- length + 1
    },
    merge = function(vector, index=NULL) {
      if (class(vector) != "Vector")
        stop("argument must be of class \"Vector\".")
      if (!is.null(index)) {
        values <<- c(values, vector$getValues()[index])
        length <<- length + length(index)
      }
      else {
        values <<- c(values, vector$getValues())
        length <<- length + vector$getLength()
      }
    },
    insert = function(value, position) {
      values <<- c(values[1:(position -1)], value, values[position:length(values)])
    },
    getValues = function() {
      return(values)
    },
    getLength = function() {
      return(length)
    },
    toMatrix = function() {
      return(matrix(values, nrow=length, byrow=T))
    }
  )
)
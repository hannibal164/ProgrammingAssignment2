## So what should happen here is makeCacheMatrix creates the object invm, and then cacheSolve checks to see if this exists. If it is, it returns it.

makeCacheMatrix <- function(x = matrix()) {
  
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setting.inv <- function(solve) invm <<- solve
  get.inv <- function() invm
  list(set = set, get = get,
       setting.inv = setting.inv,
       get.inv = get.inv)

}


## The next function checks to see if the inverse is cached for the object. 
# If it is, then it returns that inverse, otherwise it calculates the inverser

cacheSolve <- function(x, ...) {
  cachemean <- function(x, ...) {
    invm <- x$get.inv()
    if(!is.null(invm)) {
      message("getting cached data")
      return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setting.inv(invm)
    invm
  }
}

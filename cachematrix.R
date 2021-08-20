#makeCacheMatrix function
#Changes: m to SValue; mean to solve
makeCacheMatrix <- function(x = matrix(sample(1:50,9),3,3)) {
  SValue <- NULL
  set <- function(y) {
    x <<- y
    SValue <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) SValue <<- solve
  getsolve <- function() SValue
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#cacheSolve function
#Changes: m to SValue; mean to solve
cacheSolve <- function(x, ...) {
  SValue <- x$getsolve()
  if(!is.null(SValue)) {
    message("Getting Inversed Matrix")
    return(SValue)
  }
  data <- x$get()
  SValue <- solve(data, ...)
  x$setsolve(SValue)
  SValue
}
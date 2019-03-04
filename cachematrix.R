## The makecachematrix function creates a matrix object that can cache it's inverse. 
## The cachesolve function calculates the inverse of the matrix given by makecachematrix.
## If the inverse of makecachematrix has already been calculated and is the same,
## the cachesolve function just finds the inverse from the cache.

#makecachematrix

#create a special "matrix" object that can cache it's inverse

makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv =getinv)
}

#cacheSolve

#computes the inverse of the special matrix created by makecachematrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

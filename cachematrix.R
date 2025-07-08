## The following functions to solve and store the inverse of a matrix is based off of the 
## vector/mean example provided in Programming Assignment 2: Lexical Scoping (copied below)

# Create a special matrix object that can cache its inverse
# Assume supplied matrix is always invertible
makeCacheMatrix <- function(x = matrix()) {
  # To store the cached inverse
  inv <- NULL
  
  # Returns a list of fcns that set (the matrix)/get (the matrix)/setinverse/getinverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix above or retrieves the cached inverse
cacheSolve <- function(x, ...) {
  # Uses the special matrix from makeCacheMatrix and uses getinverse to see if a cached inverse exists
  # Avoids recalculation if data hasn't changed
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(inv)
  m
}

## ---------- EXAMPLE FROM ASSIGNMENT ---------- ## 
makeVector <- function(x = numeric()) {
  # To store the cached inverse
  m <- NULL
  
  # Returns a list of fcns that set (the vector)/get (the vector)/setinverse/getinverse
  # Without <<-, set and setinverse would create local copies of x and m and not modify the original
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheMean <- function(x, ...) {
  # Uses the special vector from makevector and uses getmean to see if a cached mean exists
  # Avoids recalculation if data hasn't changed
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
 # Matrix inversion and caching the inverse via function
 # The goal here is to save time and power by first 
 # caching a matrix and then computin its inverse.
 # Need to:
 #      1. Set a vector
 #      2. Get the value of the vector
 #      3. Set the mean
 #      4. Get the mean value
 # The second function then checks if this is cached
 # and uses it if it is.

makeCacheMatrix <- function(x = matrix()) {
  # For X return an inverted matrix
  # used for the cacheSolve function
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(solve) inv <<- solve 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  # If inverse exists, pull cache and skip compute
  if (!is.null(inv)){
    message("Retrieving cached data")
    return(inv)
  }
  # Else, calculate inverse 
  mtest.data = x$get()
  inv = solve(mtest.data, ...) # solve() inverts square matrices
  # Sets inverse in cache.
  x$setinv(inv)
  
  return(inv)
}

test = function(mtest){
  # mtest: square matrix
  
  # First run, solves inver
  CachedMatrix <- makeCacheMatrix(mtest)
  start.time <- Sys.time()
  solved <- cacheSolve(CachedMatrix)
  duration <- Sys.time() - start.time
  print(duration)
  
  #Second run, calls cache
  start.time <- Sys.time()
  solved <- cacheSolve(CachedMatrix)
  duration2 <- Sys.time() - start.time
  print(duration2)
}


 # Test Results
 # mtest <- runif(1000000, min = -1000000, max = 1000000)
 # mtest <- matrix(mtest, nrow = 1000, ncol = 1000)
 # test(mtest)
# Time difference of 0.1095681 secs
# Retrieving cached data
# Time difference of 0 secs

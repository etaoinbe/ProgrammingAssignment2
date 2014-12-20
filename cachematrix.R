## This function creates a special "matrix" object that can cache its inverse.
# the definition of a matrix's inverse is that 
# the product of the matrix and its inverse is the identity matrix, if the inverse exists.
# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 
# set the value of the vector
# get the value of the vector
# set the value of the inverse 
# get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  cachedvalue <- NULL
  set <- function(y) {
    x <<- y
    cachedvalue <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedvalue <<- inverse
  getinverse <- function() cachedvalue
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve  retrieves 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}

# do uncached operations, cahced operations twice and compare it all manually 
testcachesolve <- function()
{
  m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  i <- solve(m) 
  i %*% m == diag(nrow = nrow(m), ncol = ncol(m))
  u <- i %*% m
  
  m2 <- makeCacheMatrix (m)
  i2 <- cacheSolve(m2)
  i3 <- cacheSolve(m2)
  i2 %*% m == diag(nrow = nrow(m), ncol = ncol(m))
  u2 <- i2 %*% m
  list(m=m,m2=m2,i=i,i2=i2,i3=i3,u=u,u2=u2)
}




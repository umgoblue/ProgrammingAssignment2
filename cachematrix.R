## These functions solve the inverse of a matrix and cache it so that if it is needed later the inverse need not be re-computed.

## The first function creates a special 'matrix', which is actually a list. The function takes a matrix as an argument.
## It returns a list of four functions. The first function, 'set' allows you to change the value of the cached matrix itself.
## The second function, 'get' retrieves the matrix. The third and fourth functions, 'getInverse' and 'setInverse' work similarly.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function solves the inverse of the matrix or else returns the value if it is already cached. Given a 'special' matrix x of
## the type created by the first function, cacheSolve first checks to see if the list entry for getInverse is NULL. If it is not null
## then the inverse of the matrix has been stored at that entry and it just returns that cached value. If the entry is NULL then
## cacheSolve solves computes the inverse of the matrix, stored at the list entry 'get' and caches it for retrieval later.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- (solve(x$get(),...))
  x$setInverse(i)
  i  ## Return a matrix that is the inverse of 'x'
}
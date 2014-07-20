## makeCacheMatrix creates a special matrix that can cache its inverse
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, cache Solve should retrieve the inverse from the cache.

z<-matrix(rnorm(16),nrow=4,ncol=4)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      print(list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse))
}

makeCacheMatrix(z)

cacheSolve <- function(x, ...) {
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

cacheSolve(z)

# testing
amatrix<-makeCacheMatrix(z,nrow=2,ncol=2))
amatrix$get()
amatrix$

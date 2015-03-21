## creates a special "matrix" with is a list containning a functions:
##    set --- set new matrix
##    get --- get the matrix
##    setinverse --- set the inverse of the matrix
##    getinverse --- get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## compute the inverse of the special "matrix", created by makeCacheMatrix 
## The function uses the cache, so if inverse has already been calculated for this "matrix"
##    the inverse will be retrieved from cache
cacheSolve <- function(x, ...) {
   inv <- x$getinverse
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get
   inv <- solve(data, ...)
   x$setninverse(inv)
   inv
}

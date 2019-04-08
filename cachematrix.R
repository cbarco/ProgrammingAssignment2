## These are my functions for CacheMatrix of Coursera R Programming
## Week 3 Assignment; April 7th of 2019; GitHub user: cbarco

## This function is used to create a matrix were we can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #inv as NULL will hold matrix inverse
  set <- function (y) { #assign a new set to the function
    x <<- y #and this matrix value goes to parent environment
    inv <<- NULL #reset inv to NULL
  }
  get <- function() x #the get function is created
  setinverse <- function (inverse) inv <<- inverse #the value of inv in the parent environment
  getinverse <- function () inv # the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #I refer to the functions with $ operator
  }


## After we computed the function that inverses de matrix, we will retrieve the inverse from the cache with cacheSolve
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve (data, ...)
  x$setinverse (inv)
  inv
}

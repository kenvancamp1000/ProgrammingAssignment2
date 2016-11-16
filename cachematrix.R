## The first function creates a matrix that can cache the 
## inverse of itself

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL 
  }
  get <- function() x
  setInv <- function(matrixInverse) inv <<- matrixInverse
  getInv <- function() inv
  list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## This function checks if a mean has been calculated and
## if not, it calculates the mean created with the above function.
## If a mean has already been calculated, it takes the mean from the cache.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv      
}

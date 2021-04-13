## makeCacheMatrix creates a special list object containing four functions
## which set and return the values of a matrix and its inverse. Use of the <<-
## operator creates a cache pointer to the inverse to ease system resources.

makeCacheMatrix <- function(x = matrix()) {
  # Initalize inverse variable for new cache matrix object
  inverse <- NULL
  # Set the matrix for cache matrix object based on function parameter
  # and clear any previously cached inverse stored in the object
  set <- function(m)
  {
    inverse <<- NULL
    x <<- m
  }
  # Return the matrix for cache matrix object
  get <-function() {x}
  # Set the inverse of the matrix in the cache matrix object
  setinverse <-function(i){
    inverse <<- i
  }
  # Return the inverse of the matrix in the cache matrix object
  getinverse <- function() {inverse}
  # Return an object of type cache matrix as a list of four set/get functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Find the value of the inverse of a matrix and cache its value

cacheSolve <- function(x, ...) {
  # Retrieve the inverse value from x of type makeCacheMatrix
  inverse <- x$getinverse()
  # If a value is stored, return it from cache
  if(!is.null(inverse)){
    message("Retrieving cached inverse value.")
    inverse
  }
  else {
    # If not, get the matrix from the object
    data <- x$get()
    # Find the inverse using solve()
    inverse <- solve(data, ...)
    # Set the new inverse value
    x$setinverse(inverse)
    #return the inverse
    inverse
  }
}

# Make a function for calculating the inverse of matrix, assuming all matrix are inversible 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  #Initialize m to be an empty list
  set <- function(y) {  #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x   #get the value of the matrix
  setinverse <- function(solve) m <<- solve    #set the value of the inverse matrix
  getinverse <- function() m     #get the value of the inverse matrix
  list(set = set, get = get,     #return a list with the 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}

# Make a cache function to store the result of the inverse matrix and return the result without calculating it
cacheSolve <- function(x, ...) {
  m <- x$getinverse()    #m is the result from function getinverse
  if(!is.null(m)) {  #if there is a stored value in the cache
    message("getting cached data")  #print the message
    return(m)  #and return the inverse matrix without calculating it
  }
  data <- x$get()
  m <- solve(data, ...) 
  x$setinverse(m)
  m             #when there is no result stored, calculate the inverse matrix
}

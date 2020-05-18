## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing matrix inverse to NULL
  
  i <- NULL
  
  ## Setting the matrix
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  ## Getting the matrix
  
  get <- function(){
    x
  }
  
  ## Setting matrix inverse
  
  setinverse <- function(inverse){
    i <<- inverse  
  }
  
  ## Getting matrix inverse
  
  getinverse <- function(){
    i
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special matrix. If the inverse has
## already been calculated then it retrieves the inverse from the cache

cacheSolve <- function(x,...) {
  ## Return inverse matrix of x
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x
  i
}

## An example matrix

x <- matrix(c(5,10,6,7,9,2,2,6,7), nrow = 3, ncol = 3, byrow = TRUE)
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)

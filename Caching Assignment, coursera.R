makeCacheMatrix <- function(x = matrix()) {
    
  inv <- NULL
  #this sets the inv value to NULL as we have no inverse matrix yet in the cache as nothing has been calculated
  
  
  set <- function(y) {
      
    x <<- y
    inv <<- NULL
  }
  #this sets the value of x, to whatever the user wants to put in (remember that 'set' is a function within makeCacheMatrix)
    
      get <- function()x 
      setinverse <- function(inverse) inv<<- inverse
      #here you are JUST STORING THE INVERSE, as the variable 'inverse', so that it can be used for further uses beyond this script
      getinverse <- function()inv
      list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
  
      #setinverse is used for caching with the function 'cacheSolve' and solves the inverse, then it updates the inv variable with the value obtained with the solve function
}

cacheSolve <-  function(x, ...)
  
    inv <- x$getinverse
        if(!is.null(inv)) {
                message("getting cached data for inversed matrix")
                return(inv)
        }
    
    data <- x$get
    inv <- solve(data, ...)
    x$setinverse(inv)
    #uses the function 'setinverse' within x and its function, to both solve the inverse of the inv
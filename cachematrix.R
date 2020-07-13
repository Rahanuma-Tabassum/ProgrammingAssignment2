## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix, gets its value
## set the value of the inverse matrix created with inverse function
## and gets its value

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL ##initializing x as function input and inv as NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
    
    ###Assign the input argument to the x object in the parent environment, and
    ##Assign the value of NULL to the inv object in the parent environment. 
    ##This line of code clears any value of inv that had been cached by a prior 
    ##execution of cacheSolve function.
  }
  get <- function() 
  {
    x  ##Since the symbol x is not defined within get function, 
    ##R retrieves it from the parent environment of makeCacheMatrix function
  } 
  
  setInverse <- function(inverse)  
  {
    inv <<- inverse
  }
  
  getInverse <- function() 
  {
    inv
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  ##this creates a new object by returning a list
  ##When the function ends, it returns a fully formed object of 
  ##type makeCachematrix function to be used by downstream R code
}



## Write a short comment describing this function
##cacheSolve function calculates the inverse matrix
## created with the makeCacheMatrix function
## It first checks to see if the inverse matrix has already
## created. If so, it gets the inverse matrix from the cache
## and skips its creation. Otherwise, it create the
## inverse matrix of the mat and sets the value of the inverse
## matrix in the cache using setInverse function.


cacheSolve <- function(x, ...)
{
  ##Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  ##now it checks to see whether the result is null or not
  
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  ##If the result of !is.null(i) is FALSE, cacheSolve function gets the matrix from the
  ##input object, calculates a solve function, uses the setInverse function on the 
  ##input object to set the inverse in the input object, and then returns the value 
  ##of the inverse to the parent environment by printing the inverse object
  
  mat <- x$get()
  inv <- solve(mat, ...) ##calculate inverse and assign to inv
  x$setInverse(inv)
  inv
}

### Assignment: Caching the Inverse of a Matrix ###

## The file include a pair of functions that cache the inverse of a matrix.

## Caching the inverse of matrix can be usefull because of costly computation of matrix inversions



# function to creating a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  # set the value of the matrix
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  set.inverse <- function(solve) m <<- solve
  # get the value of the inverse
  get.inverse <- function() m
  
  # return a list of functions 
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


# function to computing the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) 
{

  m <- x$get.inverse()
  
  # if the inverse has already been calculated and the matrix has not changed
  # - get the inverse from the cache and skip the computation.
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  # calculating the inverse of the matrix
  data <- x$get()
  m <- solve(data)
  # setting the value of the inverse in the cache via the set.inverse function
  x$set.inverse(m)
  
  # return a matrix that is the inverse of 'x'
  return(m)
  
}

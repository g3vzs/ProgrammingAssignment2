## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a cache marix object that can be used to
## repeatably solve the inverse of the matrix, but only
## calculates the inverse once.
## 



## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
      cache_inverse <- NULL ##begins by setting the mean to NULL as a placeholder for a future value
      set <- function(y) {
            x<<-y
            cache_inverse <<-NULL ## defines a function to set the vector, x, to a new vector, y, 
      }                           ## and resets the mean, m, to NULL (like in the mean value example)
      get <- function() x
      set_inverse <- function(inverse) cache_inverse <<- inverse 
      get_inverse <- function() cache_inverse
      list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse) 
      ## returns the 'special vector' containing all of the functions defined above
}


## Write a short comment describing this function
## cacheSolve reates a cacheMatrix object for an invertale matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$get_inverse()
      if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...) ## as indicated in the instructions solve returns the inverse matrix
      x$set_inverse(inverse)
      inverse
}

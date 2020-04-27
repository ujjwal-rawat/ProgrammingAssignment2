## The functions makeCacheMatrix and cacheSolve helps to cache the inverse
## of a matrix to speed up the computations where inverse of a matrix is
## required repeatedly.

# This function provides the helper functions to create and manage the cache
# for a given matrix
makeCacheMatrix <- function(x = matrix()) {
  
  #inverse of the matrix is initialized a NULL
  matrix_inverse <- NULL
  
  #Stores the value of the matrix. Inverse of matrix is stored as NULL
  set_matrix <- function(mat){
    x <<- mat
    matrix_inverse <<- NULL
  }
  
  # Returns the cached matrix
  get_matrix <- function(){
    x
  }
  
  #Stores the inverse of the cached matrix
  set_inverse <- function(inv){
    matrix_inverse <<- inv
  } 
  
  #Returns the inverse of the cached matrix
  get_inverse <- function(){
    matrix_inverse
  } 
  
  list(set_matrix = set_matrix, get_inverse = get_inverse, set_inverse = set_inverse, get_inverse = get_inverse)
}

# Returns the inverse of the cached matrix. If the inverse is not available,
# it is calculated and cached before returning
cacheSolve <- function(x, ...) {
  #get the cached inverse of the matrix
  matrix_inverse <- x$get_inverse()
  
  #if inverse of matrix exists, return it
  if(!is.null(matrix_inverse)){
    message("getting cached data")
    return(matrix_inverse)
  }
  
  mat <- x$get_matrix()
  
  #Calculate the inverse of the matrix
  matrix_inverse <- solve(mat)
  
  #Cahe the inverse of the matrix
  x$set_inverse(matrix_inverse)
  
  matrix_inverse      
}

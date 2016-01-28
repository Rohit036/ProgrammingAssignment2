## Put comments here that give an overall description of what your
## functions do
#  The first function is used to store(set) or get the values of a matrix and
#  it's inverse . The second function is used to either retreive the already 
#  set(cached) value of the inverse or will store it if not set in the first place  
 ## Author @ paresh nayyar
## Write a short comment describing this function
#  This function 'makeCacheMatrix' returns a list of functions 
#  that are defined to store(set) as well as retreive(get) the value of a 
#  invertible square matrix and it's inverse 

makeCacheMatrix <- function(x = matrix()) {
 
inverse <- NULL
  
  set_matrix <- function(y)      # set the value of the matrix
  {
    x<<-y
    inverse <- NULL
  }
  get_matrix <- function() 
  { 
    x                            # get the value of the matrix 
  }
  
  set_inverse <- function(inv) 
  {
    inverse <<- inv              # set the inverse of the matrix
  }
  
  get_inverse <- function(x) 
  { 
    inverse                      # get the inverse of the matrix 
  }
  
  # returns the list of all the defined functions
  list(set = set_matrix , get = get_matrix , setinverse = set_inverse
        , getinverse = get_inverse)    
  }

## Write a short comment describing this function
#  This function 'cacheSolve' is used to calculate the inverse of the defined 
#  matrix in the first function . If the inverse is already set in the 
#  first function , this function will just return the cached 
#  inverse value otherwise it will calculate the inverse and will also 
#  cache it for further use  

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  mat1 <- x
  mat2 <- matrix$get()
  inverse <- matrix$getinverse()
  
  # condition to check if the mtrix defined in the first place  
  # and the one passed in this argument is same or not and also
  # whether the inverse value is previously cached or not 
  if( identical(mat1,mat2) && (!is.null(inverse)))                  
  {
    message("Matrices Are Identical , Cached Inverse Returned")
    return (inverse)
  }  else if (identical (mat1,mat2) && (is.null(inverse)))
    {
    message("Matrices Are Indentical ,  Inverse Calculated and Cached ")
    data <- matrix$get()
    new_inverse <- solve(data)
    matrix$setinverse(new_inverse)
    return (new_inverse)
  }else if(!identical(mat1 , mat2))  
  {
   # if the matrix defined in the first functions does not match with one passed
   # in this function then it will return the inverse of this new matrix 
    message("Matrices Are Not Identical , Inverse of the New Matrix Returned  ")
    return (solve(x))
   }
}

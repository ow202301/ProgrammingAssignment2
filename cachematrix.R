#a pair of functions that cache the inverse of a matrix

#set a function that creates a special matrix object
makeCacheMatrix <- function(m = matrix()){
  
  #initialize the inverse property
  i <- NULL
  
  #set the matrix
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  
  #get the matrix
  get <- function(){
    
    #return the matrix
    m
  }
  
  #set the inverse of the matrix
  setInverse <- function(inverse){
    i <<- inverse
  }
  
  #get the inverse of the matrix
  getInverse <- function(){
    
    #return the inverse property
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  
  #a matrix that is the inverse of x
  m <- x$getInverse()
  
  #return the inverse if it's already set
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  
  #compute the mattrix if it's not set
  data <- x$get()
  
  #calculate the inverse using solve function
  m <- solve(data) %*% data
  
  #set the inverse of the matrix
  x$setInverse(m)
  
  #return the matrix
  m
}













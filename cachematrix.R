## These functions compute the inverse of a matrix and cache this matrix for future use

## This function makes a "cachematrix", which is a list that contains a function to set the matrix, get the matrix, set the vaue of the inverse and get the value of the inverse.

makeCacheMatrix<- function(x = matrix()) {   #This creates a new matrix
  s<- NULL                                   #This sets s to NULL within the function
  set<-function(y) {
    x<<-y
    s<<- NULL                             # This sets s equal to NULL in the global environment
  }
  get<-function() x                          #this defines the original matrix
  setinverse<-function(solve) s <<- solve    #this calculates the inverse matrix and assigns it to s in the global environment
  getinverse<-function() s                   #this returns the inversed matrix
  list(set = set, get=get,                   #this prints a list of $set, $get, $setinverse and $getinverse
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function calculates the inverse of the matrix created with the above function, but first checks if it has been calculated earlier
## if it has already been calculated, it gets the inverse from the cache and skips the calculation
## else, t calculates the inverse of the mean, and sets the value of the inverse in the cache using the setinverse function

cacheSolve <- function(x, ...) {          #This function tries to find a matrix previously defined by makeCacheMatrix
  s <- x$getinverse()                     #S is assigned a the value of getinverse() as defined in the MakeCacheMatrix 
  if(!is.null(s)){                        #If the makeCacheMatrix function has been run for the matrix, s is not equal to null
    message("getting cached data")      #prints the message "getting cached data
    return(s)                           #print the inverse matrix previously cached
  }
  data<-x$get()                           #gets the value of the matrix by assigning it to dta
  s <- solve(data, ...)                   #this computes the inverse of the matrix and assigns it to s
  x$setsolve(s)                           #this gets the value of s
  s                                       #this prints the value of s
}

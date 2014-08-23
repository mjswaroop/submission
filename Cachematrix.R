makeCacheMatrix <- function(x = matrix()) { #We take input in Matrix form only
              inv <- NULL                 #Just like in the example inv will store the inverse 
                                          #of the matrix and is reset to NULL everytime makeCacheMAtrix
                                          #is called
  
  set <- function(y) {
             x <<- y
            inv <<- NULL
  }
  get <- function() {x}                 #This function returns the orginal matrix
  
  
  
  setinv <- function(inverse) {         #called when cachemin is invoked for first time and
            inv <<- inverse             #will store the inverse in enviroment of MakeCacheMatrix
  }
  
  getinv <- function() {                #Used to return the inverse from memory
            inv
  }
  
}

cacheSolve <- function(x, ...) {        #the input is a MAtrix created by makeCacheMatrix
  inv <- x$getinv()                     #acess the specific matrix and gets the value of inverse
  
  
  if(!is.null(inv)) {                   #If inverse was already stored i.e not NULL then it
    message("getting cached data")      #retrives the data from memory and prints it
    return(inv)                         #this function ends here if inverse was already calculated
  }
  data <- x$get()                       #Reached only when if loop was not executed
  inv <- solve(data, ...)               #We calculate the inverse 
  x$setinv(inv)                         #Store the value back in x i.r in makeChacheMatrix Enviroment
  inv                                   #Returs the inverse to be printed
}
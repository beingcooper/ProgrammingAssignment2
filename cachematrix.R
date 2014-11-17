## The program calculates the inverse of a square matrix.
## However to save time, the value of the inversed is cached,
## thus if the data of the matrix remains unchanged then
## the cached value of inverse is used instaed to calculating 
## it again. This helps our program to save computation time.

## makeCacheMatrix function takes a matrix x as input and use the following functions
## set to set the data of the matrix
## get to get the data of the matrix
## setinv to set the inverse of the matrix
## getinv to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    inv<-NULL
    
    set<-function(y){
        
      x<<-y
      inv<<-NULL
    }
    
    get<-function() x
    
    setinv<-function(inverse){
      
        inv<<-inverse
    }
    
    getinv<-function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve observes if the inverse has already been calculated previously, 
## if yes, the inverse if returned, if no, inverse is first calculated using
## solve() function and then calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    
    if(!is.null(inv)){
      
          message("getting cached data.")
          return(inv)
    }
    
    data<-x$get()
    
    inv<-solve(data)
    x$setinv(inv)
    inv
}

# test run...

# Creating matrix
# > data<-rbind(c(1,3),c(3,1))
# > x<-makeCacheMatrix(data)

# Printing matrix
# > x$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    3    1

# Calling cacheSolve for the first time
# > cacheSolve(x)
# [,1]   [,2]
# [1,] -0.125  0.375
# [2,]  0.375 -0.125

# Calling cacheSolve for second time (cached value used)
# > cacheSolve(x)
# getting cached data.
# [,1]   [,2]
# [1,] -0.125  0.375
# [2,]  0.375 -0.125

# The two functions below are used to calculate the 
# inverse of a matrix. It is important to note that if the matrix has been
# cached before, the inverse of a matrix will be returned from the cache
# rather than re-computing it; which a nice way to save time especially when
# working with large datasets and repetetive analysis such as looping
# over the same processs over and over again.


# The makeCacheMatrix function shown below creates a special "matrix", 
# which is really a list containing a function to
       #1. set the value of the matrix
       #2. get the value of the matrix
       #3. set the inverse of the matrix
       #4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) m<<- inverse
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## The cacheSolve function below calculates the inverse of the special
# "matrix" created with makeCacheMatrix function.
# In doing so, it first checks if the inverse of the matrix
# has been cached. If yes, it gets the inverse from the cache
# and proceeds to the next computation. 
# If it has not been cached, however,
# it calculates the inverse of the data
# Moreover,it sets the inverse matrix in the cache by
# using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}

# Demonstration:

fish<-makeCacheMatrix()  # simply storing a function in a variable

 # Let us store a 4 by 4 matrix
fish$set(matrix(c(2, 3, 1, 5,1, 0, 3, 1, 0, 2, 
                  -3, 2, 0, 2, 3, 1),4,4, byrow=TRUE))

cacheSolve(fish) # computes the inverse of the matrix for the first time and gives


      [,1] [,2] [,3] [,4]
[1,]   18  -35  -28    1
[2,]    9  -18  -14    1
[3,]   -2    4    3    0
[4,]  -12   24   19   -1

# if we repeat the command again       
cacheSolve(fish) # we get the message and the inverse of the matrix
                 # since it got it from the catche, R did not recompute the inverse

      getting cached data
[,1] [,2] [,3] [,4]
[1,]   18  -35  -28    1
[2,]    9  -18  -14    1
[3,]   -2    4    3    0
[4,]  -12   24   19   -1


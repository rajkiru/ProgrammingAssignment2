## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# function to set cache value , get cache value, set inverse, get inverse and get environment location
  	mat_cache<-NULL        # initialize non-cache variable
 	 print("without cache,  environment value is")
 	 print(environment())
 	 setmatrix<-function(y){
 	   x<<-y   # set matrix to argument
  	   mat_cache<<-NULL   # set variable as cache
  	   message(" Setting variable in cache")
   	   #print(environment())
  	   getenv()
	  }
  	getmatrix<-function() x   # get matrix value
 	setmatrixinv<-function(solvemat) mat_cache<<- solvemat  # 	assign matrix to cache
  	getmatrixinv<-function(){  
   	getenv()
    	mat_cache  # return from cache
  	}
  	getenv <- function()  {  # to identify environment from where fn is called
   	 print("Parent env is")
   	 print(parent.env(environment()))
   	 print ("current cache environment")
    	 print(environment())
  	} 
    #environment()
    list(setmatrix=setmatrix, getmatrix=getmatrix,
       setmatrixinv=setmatrixinv,
       getmatrixinv=getmatrixinv, getenv=getenv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mat_cache<-x$getmatrixinv() # get inverse mat from cache
  	if(!is.null(mat_cache)){
   		 message("getting cached data")
   		 return(mat_cache)
		}
 	 # if cache is NULL
	 matrix<-x$getmatrix() # get matrix value from variable
  	 mat_cache<-solve(matrix, ...) # inverse calculation
 	 x$setmatrixinv(mat_cache) # assign inverse to cache
 	 mat_cache # return inverse
	}



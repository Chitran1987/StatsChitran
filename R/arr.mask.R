#Write a program to mask an array with matrices
arr.mask <- function(arr, mask, drop.dat=F){
  ###array should be rank-3
  if( length(dim(arr)) != 3 ){
    stop('arr should be a rank-3 tensor')
  }
  ###mask should be rank-2/matrix
  if( !is.matrix(mask) ){
    stop('mask should be a matrix')
  }
  ###dimension of arr[,,k] and mask should be the same
  if( !all( dim(arr[,,1]) == dim(mask) ) ){
    stop('array slices "arr[,,p]" and "mask" should have same dimensionality ')
  }
  ###mask should be a logical matrix
  if( !is.logical(mask) ){
    stop('mask needs to be a logical matrix')
  }
  ###drop.dat should be logical
  if( !is.logical(drop.dat) || ( length(drop.dat)!=1 ) ){
    stop('drop.dat needs to be a single boolean bit')
  }

  #code
  N <- dim(arr)[3] #No of slices
  res1 <- mat.mask(arr[,,1], mask = mask, drop.dat = drop.dat)
  arr.res <- array(data=NA, dim = c(dim(res1), N))
  if(drop.dat == T){
    for (i in 1:N) {
      arr.res[,,i] <- mat.mask(arr[,,i], mask = mask, drop.dat = drop.dat)
    }
    return(arr.res)
  }else{
    for (i in 1:(N-2)) {
      arr.res[,,i] <- mat.mask(arr[,,i], mask = mask, drop.dat = drop.dat)
    }
    arr.res[,,N-1] <- arr[,,N-1]
    arr.res[,,N] <- arr[,,N]
    return(arr.res)
  }

}

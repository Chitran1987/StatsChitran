#Write a function called matrix.mask to mask certain values of a matrix w.r.t a mask
mat.mask <- function(org, mask, drop.dat=F){
  #org and mask should be matrices(rank-2) and their dimensions and ranks should match
  if( !is.matrix(org) || !is.matrix(mask)){
    stop('Both org and mask should be matrices')
  }
  if(!all( dim(org) == dim(mask) )){
    stop('Dimesnions of org and mask should be the same')
  }
  #mask.mat should be boolean
  if( !is.logical(mask) ){
    stop('mask needs to be a logical matrix')
  }
  #org.mat should be numeric
  if(!is.numeric(org)){
    stop('org needs to be a numeric matrix')
  }
  #drop.dat needs to be a boolean scalar
  if( !is.logical(drop.dat) || ( length(drop.dat)!=1 ) ){
    stop('drop.dat needs to be a single boolean bit')
  }
  #code
  if(!drop.dat){
    return(org*mask)
  }else{
    ####generate matrix M
    M <- org*mask
    #### Sum each row in matrix M
    #### indexes of rows where rowsums are not equal to zero
    #### If min(ind) > 1, then remove rows with indexes 1:(ind - 1)
    #### If max(ind) < (no. of Rows), then remove rows with indexes (no. of Rows) + 1: (no. of Rows)
    #### remove the rows with those indexes
    #### Then proceed to do the same with Columns
    #### This will draw a box and give us the minimum size of the image that we must keep to have the requisite image
    ##This is the rows part
    RSums <- rowSums(M)
    ind <- which(RSums != 0)
    N <- nrow(M)
    if( min(ind) > 1 ){
      M <- M[ - (1:(min(ind) - 1) ), ]
    }
    RSums <- rowSums(M)
    ind <- which(RSums != 0)
    N <- nrow(M)
    if(max(ind) < N){
      M <- M[-( (max(ind)+1):N ),]
    }
    ##This is the columns part
    CSums <- colSums(M)
    ind <- which(CSums != 0)
    N <- ncol(M)
    if( min(ind) > 1 ){
      M <- M[ ,- (1:(min(ind) - 1) ) ]
    }
    CSums <- colSums(M)
    ind <- which(CSums != 0)
    N <- ncol(M)
    if(max(ind) < N){
      M <- M[,-( (max(ind)+1):N )]
    }
    return(M)
  }
}

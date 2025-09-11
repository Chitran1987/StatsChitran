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
    #M <- org*mask
    #### Sum each row in matrix M
    #### indexes of rows where rowsums are not equal to zero
    #### If min(ind) > 1, then remove rows with indexes 1:(ind - 1)
    #### If max(ind) < (no. of Rows), then remove rows with indexes (no. of Rows) + 1: (no. of Rows)
    #### remove the rows with those indexes
    #### Then proceed to do the same with Columns
    #### This will draw a box and give us the minimum size of the image that we must keep to have the requisite image

    M <- org*mask
    ##This is the rows part
    row_res <- apply(mask, 1, any)
    ####while loop counting front eliminations
    dmmy.ctr <- F
    i <- 1
    while (dmmy.ctr == F) {
      dmmy.ctr <- dmmy.ctr | row_res[i]
      i <- i + 1
    }
    frnt_elm <- i-2
    ####eliminate from the front

    if(frnt_elm > 0){
      M <- M[-(1:frnt_elm),]
      mask <- mask[-(1:frnt_elm),]
    }
    ####while loop counting back eliminations
    row_res <- apply(mask, 1, any)
    dmmy.ctr <- F
    i <- length(row_res)
    while (dmmy.ctr == F) {
      dmmy.ctr <- dmmy.ctr | row_res[i]
      i <- i - 1
    }
    bck_elm <- length(row_res)-i-1

    ####eliminate from the back
    if(bck_elm > 0){
      M <- M[-((length(row_res)-bck_elm+1):length(row_res) ),]
      mask <- mask[-((length(row_res)-bck_elm+1):length(row_res) ),]
    }

    ##This is the columns part
    rm(row_res)
    col_res <- apply(mask, 2, any)
    ####while loop counting front eliminations
    dmmy.ctr <- F
    i <- 1
    while (dmmy.ctr == F) {
      dmmy.ctr <- dmmy.ctr | col_res[i]
      i <- i + 1
    }
    frnt_elm <- i-2
    ####eliminate from the front
    if(frnt_elm > 0){
      M <- M[,-(1:frnt_elm)]
      mask <- mask[,-(1:frnt_elm)]
    }
    ####while loop counting back eliminations
    col_res <- apply(mask, 2, any)
    dmmy.ctr <- F
    i <- length(col_res)
    while (dmmy.ctr == F) {
      dmmy.ctr <- dmmy.ctr | col_res[i]
      i <- i - 1
    }
    bck_elm <- length(col_res)-i-1

    ####eliminate from the back
    if(bck_elm > 0){
      M <- M[,-((length(col_res)-bck_elm+1):length(col_res) )]
      mask <- mask[,-((length(col_res)-bck_elm+1):length(col_res) )]
    }

    return(M)
  }
}

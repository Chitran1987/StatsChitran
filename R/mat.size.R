#write a program called mat.size which sizes a matrix down to its mask but doesn't etch the data
mat.size <- function(org, mask){
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

  ####code##############################################
  M <- org
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

#Write a program called plot2D.zoom
plot2D.zoom <- function(arr, center, Del_X, Del_Y, pl = T ){
  ###arr is the rank-3 tensor, three slice which will be zoomed on
  ###center is the center (X,Y) of the zoom window
  ###Del_X is the X-length of the Zoom box
  ###Del_Y is the Y-length of the Zoom box

  ####error checking
  ###arr is a rank-3 tensor which is also 3-slice
  if(get.strct(arr) != "Array"){
    stop('"arr" has to be a rank-3 array')
  }
  if(length(dim(arr)) != 3){
    stop('"arr" has to be a rank-3 array')
  }
  if(dim(arr)[3] != 3){
    stop('"arr" has to be a rank-3 array')
  }
  ###center has to be a two element vector
  if( (!is.numeric(center)) || !is.atomic(center) ||  !(length(center) == 2) ){
    stop('"center" has to be a 2-element numeric vector')
  }
  ###Del_X and Del_Y has to be numeric scalars
  if(!is.numeric(Del_X) || !is.numeric(Del_Y) || !(length(Del_X) == 1) || !(length(Del_Y) == 1) ){
    stop('"Del_X" and Del_Y have to be numeric scalars')
  }
  ###pl has to be a boolean bit
  if(!is.logical(pl) || !(length(pl) == 1)){
    stop('"pl" has to be a boolean bit')
  }
  ####code
  mid <- center
  Mx <- (arr[,,2] >= mid[1] - Del_X/2) & (arr[,,2] <= mid[1] + Del_X/2) ####The X-mask
  My <- (arr[,,3] >= mid[2] - Del_Y/2) & (arr[,,3] <= mid[2] + Del_Y/2) ####The Y-mask
  Msk <- Mx & My ####The total mask

  arr.res <- arr.mask(arr, mask=Msk, drop.dat = T) ####Calculate the resultant array

  if(pl){
    plot2D.arr(arr.res)
  }
  return(arr.res)

}

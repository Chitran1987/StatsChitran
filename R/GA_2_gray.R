#write program for converting RGBA values to grayscale
GA_2_gray <- function(gray.ch, alp.ch, bg = 1){
  ##error checking###################################
  #check if gray and alp are numerics
  #this eliminates dataframes, lists and functions. Only matrixes, vectors or arrays allowed
  if( ( !(is.numeric(gray.ch)) | !(is.numeric(alp.ch)) ) == T ){
    stop('gray.ch and alp.ch have to be numeric')
  }
  #check if gray and alp are the same type of structure
  cmp_vec <- c(get.strct(gray.ch), get.strct(alp.ch))
  if( all(cmp_vec == cmp_vec[1]) == F ){
    stop('gray.ch and alp.ch should all have the same structure')
  }
  #check if dimensions of gray and alp channels are the same
  L <- list(gray.dim = dim(gray.ch))
  L$alp.dim <- dim(alp.ch)
  if( identical(L$gray.dim, L$alp.dim) == F ){
    stop('The dimensions of gray.ch and alp.ch need to be the same')
  }
  #check whether bg is a number and is less than equal to 1, provided alp == T
  if( is.numeric(bg) && (length(bg) == 1) != T ){
    stop('bg needs to be a single number')
  }
  if( ( bg > 1 ) || ( bg < 0 ) ){
    stop('The value of bg needs to lie between 0 and 1, inclusive of both values')
  }
  ######################################################

  ##core##############################################
  #calculate gray values
  gray_val <- alp.ch*gray.ch + (1 - alp.ch)*bg

  return(gray_val)
}

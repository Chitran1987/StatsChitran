#write program for converting RGBA values to grayscale
RGBA_2_gray <- function(red.ch, green.ch, blue.ch, alp.ch, bg = 1, alp = T){
  ##error checking
  #check if alp is a single boolean bit
  if( is.logical(alp) && (length(alp) == 1) == F ){
    stop('alp needs to be a single boolean bit')
  }
  #check if red, green and blue are numerics
  #this eliminates dataframes, lists and functions. Only matrixes, vectors or arrays allowed
  if( ( !(is.numeric(red.ch)) | !(is.numeric(green.ch)) | !(is.numeric(blue.ch)) ) == T ){
    stop('red.ch, green.ch and blue.ch have to be numeric')
  }
  #check if alpha channel is numeric, provided alp == T
  if( alp == T ){
    if(is.numeric(alp.ch) == F){
      stop('alp.ch needs to be a numeric')
    }
  }
  #check if red, green and blue are the same type of structure
  cmp_vec <- c(get.strct(red.ch), get.strct(green.ch), get.strct(blue.ch))
  if( all(cmp_vec == cmp_vec[1]) == F ){
    stop('red.ch, green.ch and blue.ch should all have the same structure')
  }
  #check if the structure of alp.ch is same as the others if alp == T
  if(alp == T){
    if(get.strct(alp.ch) != get.strct(red.ch)){
      stop('structure of alp.ch has to be the same as red.ch, green.ch and blue.ch')
    }
  }
  #check if dimensions of red, blue and green channels are the same
  L <- list(red.dim = dim(red.ch))
  L$green.dim <- dim(green.ch)
  L$blue.dim <- dim(blue.ch)
  if( identical(L$red.dim, L$green.dim) && identical(L$green.dim, L$blue.dim) == F ){
    stop('The dimensions of red.ch, green.ch and blue.ch need to be the same')
  }
  #check if the dimension of alp.ch are equal to the dimension of red, blue and green channels if alp == T
  if( alp == T ){
    if( identical(L$red.dim, dim(alp.ch)) == F ){
      stop('The dimesnions of the alpha channel need to be the same as the red, green and blue channels')
    }
  }
  #check whether bg is a number and is less than equal to 1, provided alp == T
  if( alp == T ){
    if( is.numeric(bg) && (length(bg) == 1) != T ){
      stop('bg needs to be a single number')
    }
    if( ( bg > 1 ) || ( bg < 0 ) ){
      stop('The value of bg needs to lie between 0 and 1, inclusive of both values')
    }
  }


  ################

  ##core#####################
  #calculate gray values
  gray_val <- 0.2989*red.ch + 0.5870*green.ch + 0.1140*blue.ch

  #ask permission if alpha is required
  if(alp == T){
    gray_val <- alp.ch*gray_val + (1 - alp.ch)*bg
  }
  return(nrm(gray_val))
}

##write the bode function
bode <- function(wL, wU, fun, pl=T, n=5*10^5){
  ##error handling
  chk1 <- formals(fun)
  if(length(chk1) != 1 ){
    stop('transfer function should only have a single argument')
  }
  w <- seq(wL, wU, by=(wU - wL)/n)
  s <- 1i*w
  Ymag <- 20*log10(abs(fun(s)))
  Yphase <- Arg(fun(s))*180/pi
  df_mag <- data.frame(w, Ymag)
  df_phase <- data.frame(w, Yphase)
  res <- list(df_mag, df_phase)
  names(res) <- c('mag', 'phase')
  if(pl == T){
    par(mfrow = c(2,1))
    plot(df_mag$w, df_mag$Ymag, log = 'x', col=rgb(0,0,1,0.5), type = 'l', lwd=2, main = 'magnitude', xlab = 'w (rad/s)', ylab = 'dB')
    grid(nx = NULL, ny = NULL, lty = 3, col = "gray")
    plot(df_phase$w, df_phase$Yphase, log = 'x',col=rgb(0,0,1,0.5), type = 'l', lwd=2, main = 'phase', xlab = 'w (rad/s)', ylab = 'Phase (deg)')
    grid(nx = NULL, ny = 20, lty = 3, col = "gray")
    par(mfrow = c(1,1))
  }
  return(res)
}

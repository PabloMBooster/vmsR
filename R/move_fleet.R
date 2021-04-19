move_fleet <- function(data_inte = data_inte, xlim=c(-86,-70), lwdArrows = 1,
                       ylim=c(-21, -3), interval = 0.9, movie.name = "movie.gif", nmax = 50, file_harbor = NULL){

  Data.Barco <- list()
  for(i in seq_along(unique(data_inte$Cod_Barco))){
    sxs <- data_inte[data_inte$Cod_Barco == unique(data_inte$Cod_Barco)[i],]
    sxs$Color <- i
    newPoint  <- vmsR:::.pointZarpe(sxs$LONGITUDE, sxs$LATITUDE)

    x0 <- newPoint$x2
    y0 <- newPoint$y2
    x1 <- rev(rev(x0)[-1])
    y1 <- rev(rev(y0)[-1])
    x2 <- x0[-1]
    y2 <- y0[-1]

    ArrowsBarco       <- data.frame(cbind(x1, y1, x2, y2))
    ArrowsBarco$Fecha <- sxs$Date.INTERP
    ArrowsBarco$Color <- sxs$Color
    Data.Barco[[i]] <- ArrowsBarco
  }


  saveGIF({
    for (i in seq_along(sort(unique(data_inte$Date.INTERP)))){

      par(mar = c(4,4,4,0.5), oma = c(0,0,0,0))
      plot(data_inte$LONGITUDE, data_inte$LATITUDE, main = sort(unique(data_inte$Date.INTERP))[i],
           xlim = xlim, ylim = ylim, col = "white", xlab = "Longitud",
           ylab = "Latitud", cex.axis = 1.5, cex.lab = 1.5, cex = 2)

      map(xlim = xlim, ylim = ylim, fill = TRUE, col = "khaki1", lty = 0, add = T)
      map(xlim = xlim, ylim = ylim, fill = FALSE, interior = TRUE, col = "grey", add = TRUE)
      if(!is.null(file_harbor)){
        text(x = file_harbor$lon,
             y = file_harbor$lat,
             labels = as.character(file_harbor$harbor), col = 4)
      }
      box(col = "grey")

      for(ii in seq_along(Data.Barco)){
        t     <- Data.Barco[[ii]]
        obs   <- t[t$Fecha == sort(unique(data_inte$Date.INTERP))[i],]

        Arrows(obs$x1, obs$y1, obs$x2, obs$y2, lty = 1, code = 2,lwd = lwdArrows,#arr.type = "curved",
               arr.length = 0.1, arr.adj = 1, col = obs$Color)
      }
    }
  }, movie.name = movie.name, interval = interval, nmax = nmax)
  return(invisible())
}

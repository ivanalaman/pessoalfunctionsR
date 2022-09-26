#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1]
piedonut <- function(x_outer, 
                     x_inner,
                     group_inner = NULL, 
                     labels_outer=NULL, 
                     labels_inner=NULL,
                     border_outer=NULL,
                     border_inner=NULL,
                     col_main = NULL, 
                     col_labels_outer=NULL,
                     col_labels_inner=NULL,
                     radius_outer = c(.7, 1),
                     radius_inner = 0.5, 
                     cex_labels_outer=NULL,
                     cex_labels_inner=NULL,
                     font_labels_outer=NULL,
                     font_labels_inner=NULL,
                     adj_labels_outer=NULL,
                     ...){
  
  group <- rep_len(group_inner, length(x_outer))
  ug  <- unique(group)
  tbl <- table(factor(group, levels=ug))
  
  x_inneraux <- split(x_inner, rep(1:3,c(tbl)))
  x_innerplot <- unlist(lapply(x_inneraux,unique))

  col <- if (is.null(col_main)){
    seq_along(ug)
    }else{
    rep_len(col_main, length(ug))
    }
    
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  
  plot.new()

  par(new = TRUE)
 # x_aux <- split(x_outer,rep(1:3,c(tbl)))
#  x_aux1 <- lapply(x_aux,mean)
#  x_aux2 <- unlist(x_aux1)
#  x_mean <- c(0,cumsum(x_aux2)/sum(x_aux2))

  pie(x_innerplot, 
      border = border_inner, 
      radius = radius_inner,
      col = col, 
      labels = NA)

  #Código para os labels 
  x_mean <- c(0,cumsum(x_innerplot)/sum(x_innerplot))
  nx <- length(x_mean)
  
  twopi <- 2*pi
  t2xy <- function(t) {
        t2p <- twopi * t
        list(x = radius_inner * cos(t2p), y = radius_inner * sin(t2p))
  }
    
  if(is.null(labels_inner)){
    labels_inner <- ug
  }
  if(is.null(col_labels_inner)){
    col_labels_inner <- 1:nx
  }
  for(i in 1L:nx){
     P <- t2xy(mean(x_mean[i + 0:1]))
     text((0.5 * P$x), (0.5 * P$y), 
          labels_inner[i],
          xpd = TRUE, 
          adj = 0.5,
          col = col_labels_inner,
          cex = cex_labels_inner,
          font = font_labels_inner)                   
 }
  
  par(new = TRUE)
  #pie(x_outer, border = border_outer, radius = radius[2L],
#      col = unlist(col.sub), labels = labels_outer,...)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })

  x_outerplot <- x_inner*x_outer

  annular(x_outerplot,
          r1=radius_outer[1L],
          r2=radius_outer[2L],
          col=unlist(col.sub),
          labels=labels_outer,
          border=border_outer,
          adj=adj_labels_outer,
          cex_labels_outer=cex_labels_outer,
          font_labels_outer=font_labels_outer)     
}

annular <- function (x, 
                     r1=1, 
                     r2=2, 
                     col=NULL,
                     labels=NULL,
                     border=NULL, 
                     adj=NULL,
                     cex_labels_outer=NULL,
                     font_labels_outer=NULL) {
    stopifnot(x>=0, r1 >=0, r2>0, r1<r2)
    x <- cumsum(x) / sum(x)
    x <- c(0,x)
    dx <- diff(x)
    nx <- length(dx)
    
    if(is.null(col)){
       col  <- 1:nx
    }
    
    plot.new()
    
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    
    if(pin[1L] > pin[2L]){ 
       xlim <- (pin[1L]/pin[2L]) * xlim
    }else{
       ylim <- (pin[2L]/pin[1L]) * ylim
    }
    
    if(is.null(labels)){
      labels <- 1:nx
    }
    
    plot.window(xlim=xlim*r2,
                ylim=ylim*r2)
    
    twopi <- 2*pi
    t2xy <- function(t) {
       t2p <- twopi * t
       list(x = r2 * cos(t2p), y = r2 * sin(t2p))
    }
    
    for (i in 1L:nx) {
       theta <- 2*pi*seq(x[i], x[i+1], length=100)
       polygon( c(r1 * cos(theta), r2 * cos(rev(theta))),
                c(r1 * sin(theta), r2 * sin(rev(theta))),
                col = col[i],
                border = border)
       
       P <- t2xy(mean(x[i + 0:1]))
       
       if(is.null(adj)){
          adj <- ifelse(P$x < 0, 1, 0)
       }
       
       lab <- as.character(labels[i])
       if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
            text(1.1 * P$x, 1.1 * P$y, 
                 lab, 
                 xpd = TRUE,
                 cex = cex_labels_outer,
                 font = font_labels_outer, 
                 adj = adj)
    }
  }
}

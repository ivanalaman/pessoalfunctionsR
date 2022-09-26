apcMatrix <- function(lfm, lev=NULL){
    nlev <- nrow(lfm)
    rn <- rownames(lfm)
    a <- attr(lfm, "grid")
    if(is.null(lev)){
        if(!is.null(a)){
            lev <- apply(a, 1, paste, collapse=":")
        } else if(!is.null(rn)){
            lev <- rn
        } else {
            lev <- as.character(1:nlev)
        }
    }
    cbn <- combn(seq_along(lev), 2)
    M <- lfm[cbn[1,],]-lfm[cbn[2,],]
    if (is.vector(M)){
        dim(M) <- c(1, length(M))
    }
    rownames(M) <- paste(lev[cbn[1,]], lev[cbn[2,]], sep="-")
    return(M)
}
 

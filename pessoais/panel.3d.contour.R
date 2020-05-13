### Obra de Walmes Zevianni!!!!!
panel.3d.contour <-
  function(x, y, z, rot.mat, distance,
           nlevels = 20, zlim.scaled, ...)
  {
    add.line <- trellis.par.get("add.line")
    panel.3dwire(x, y, z, rot.mat, distance,
                 zlim.scaled = zlim.scaled, ...)
    clines <-
      contourLines(x, y, matrix(z, nrow = length(x), byrow = TRUE),
                   nlevels = nlevels)
    for (ll in clines) {
      m <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[1]),
                            rot.mat, distance)
      panel.lines(m[1,], m[2,], col = add.line$col,
                  lty = add.line$lty, lwd = add.line$lwd)
    }
}

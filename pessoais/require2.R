require2 <- function(package, ask= TRUE, ...) {
   package <- as.character(substitute(package))
   if(!suppressWarnings(require(package=package, character.only = TRUE))) {
      install_package <- ask.user.yn.question(paste("Package ",package, " is not installed. Should it be installed?"))
      if(install_package) install.packages(pkgs=package)
   }
   require(package=package, character.only = TRUE)
}

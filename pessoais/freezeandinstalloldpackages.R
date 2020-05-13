choose_directory = function(caption = 'Select data directory'){
 #This function was copied of https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
  if (exists('utils::choose.dir')) {
    utils::choose.dir(caption = caption) 
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}

freeze_packages <- function(choosepathlibrary = NULL){
   #choosepathlibrary é um vetor que contém o endereço completo dos diretórios que se deseja congelar os pacotes. Se NULL, será congelado todos os pacotes de todas as versões do R instalado na máquina.
   #O output será um arquivo chamado requirements.txt que irá conter todos os nomes dos pacotes instalados na(s) biblioteca(s) pessoal(is).
   
   if(is.null(choosepathlibrary)){
     pathspackages <- .libPaths()     
     pathspackagespersonal <- pathspackages[-length(pathspackages)]
     } else { 
     pathspackagespersonal <- choosepathlibrary
     }
   
   packages <- unique(list.files(pathspackagespersonal))
   packaasmatrix <- as.matrix(packages)
   colnames(packaasmatrix) <- "packages" 
   
   direct <- choose_directory()
   capture.output(packaasmatrix,
                   file = paste(direct,
                                'requirements.txt',
                                sep='/'))  
}

installfreeze_packages <- function(){
     #Depois de congelado os pacotes, é hora de instalá-los na nova versão do R. Para isto, escolha o arquivo requirements.txt criado anteriormente com a função freeze_packages.
     fille <- file.choose()
     readfiles <- read.table(fille)
     apply(readfiles,1,install.packages)
}


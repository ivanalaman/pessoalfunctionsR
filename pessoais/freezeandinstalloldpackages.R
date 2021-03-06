choose_directory = function(caption = 'Select data directory'){
 #This function was copied of https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
  if (exists('utils::choose.dir')) {
    utils::choose.dir(caption = caption) 
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}

freeze_packages <- function(choosepathlibrary = NULL){
   #choosepathlibrary � um vetor que cont�m o endere�o completo dos diret�rios que se deseja congelar os pacotes. Se NULL, ser� congelado todos os pacotes de todas as vers�es do R instalado na m�quina.
   #O output ser� um arquivo chamado requirements.txt que ir� conter todos os nomes dos pacotes instalados na(s) biblioteca(s) pessoal(is).
   
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
     #Depois de congelado os pacotes, � hora de instal�-los na nova vers�o do R. Para isto, escolha o arquivo requirements.txt criado anteriormente com a fun��o freeze_packages.
     fille <- tcltk::tk_choose.files(caption='Select your requirements file!')
     readfiles <- read.table(fille)
     apply(readfiles,1,install.packages)
}


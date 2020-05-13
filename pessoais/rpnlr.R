rpnlr <- function(model, data, start, subset=NULL){
  ##-----------------------------------------------------------------------------
  ## PORÇÃO DE PROTEÇÕES DA FUNÇÃO.
  ## nome da variável dependente
  vardep <- all.vars(model[[2]])
  ## nome da variável independente, testa se é apenas uma
  varindep <- intersect(all.vars(model[[3]]), colnames(data))
  if(length(varindep)!=1)
    stop("just one independent variable is expected!")
  ## nome dos parâmetros
  parnames <- intersect(all.vars(model[[3]]), names(start))
  ## testa se os nomes dos parâmetros seguem o padrão th1, th2, ...
  test.start.names <- length(grep("^th[1-5]$", parnames))==length(parnames)
  if(!test.start.names)
    stop("in start and model formula parameter names should follow the pattern: th1, th2, ..., th5!")
  ## função que converte uma formula em uma função que retorna valores da função
  if(!is.null(subset)){
    if(length(intersect(subset, names(data)))==0)
      stop("subset variable is not present in data!")
    if(!is.factor(data[,subset]))
      stop("subset variable must be a factor!")
  }
  ## testa e adequa os nomes dos elementos dos vetores dentro da lista start
  startnames <- sapply(start,
                       function(x){
                         !(!is.null(names(x)) & all(names(x)%in%c("init","from","to")))
                       })
  if(any(startnames)){
    message("at least one start element is not named. Using the current order to name it as init, from and to.")
    for(j in which(startnames)) names(start[[j]]) <- c("init","from","to")
  }
  ##-----------------------------------------------------------------------------
  ## PORÇÃO DE OBJETOS AUXILIARES.
  ## cria objetos que serão preenchidos ao ajustar o modelo
  if(!is.null(subset)){
    aju <- vector(mode="list", length=nlevels(data[,subset]))
    names(aju) <- levels(data[,subset])
  }
  else aju <- vector(mode="list", length=1)
  assign("ajuste", aju, env=.GlobalEnv)
  ##-----------------------------------------------------------------------------
  ## PORÇÃO DE FUNÇÕES INTERNAS.
  ## Função que converte uma fórmula da nls() em uma função para predição.
  form2func <- function(formu){
    arg1 <- all.vars(formu)
    arg2 <- vector("list", length(arg1))
    names(arg2) <- arg1
    Args <- do.call("alist", arg2)
    fmodele <- as.function(c(Args, formu))
    return(fmodele)
  }
  fmodele <- form2func(model[[3]])
  ## função que é controlada e passar a curva por meio dos pontos
  nlr.draw <- function(panel){
    if(is.null(panel$subset))
      plot(panel$vindep, panel$vdep, xlab=varindep, ylab=vardep)
    else {
      da <- data.frame(vindep=panel$vindep, vdep=panel$vdep, subset=panel$subset)
      plot(vdep~vindep, data=subset(da, subset==panel$sbst),
           xlab=varindep, ylab=vardep)
    }
    vindepseq <- seq(min(panel$vindep), max(panel$vindep), length.out=101)
    listparvar <- c(list(vindepseq), panel[parnames])
    names(listparvar)[1] <- varindep
    fx <- do.call("fmodele", listparvar)
    lines(vindepseq, fx, col=2, lty=2)
    panel
  }
  ## nlsajust é excutada quando clicar no botão "Ajustar"
  nlsajust <- function(panel){
    ## ajuste do modelo não linear
    nlsstart <- panel[parnames]
    if(is.null(panel$subset)){
      da <- data.frame(vindep=panel$vindep, vdep=panel$vdep)
      names(da) <- c(varindep, vardep)
      n0 <- try(nls(panel$model, data=da, start=nlsstart))
    } else {
      da <- data.frame(vindep=panel$vindep, vdep=panel$vdep, subset=panel$subset)
      names(da) <- c(varindep, vardep, "subset")
      n0 <- try(nls(panel$model, data=subset(da, subset==panel$sbst), start=nlsstart))
    }
    ## em caso de não convergência imprime mensagem de erro
    if(class(n0)=="try-error"){
      par(usr=c(0, 1, 0, 1))
      text(0.5, 0.5, "Convergence not met!\nGet closer!", col="red", cex=2)
    } else {
      ## coloca à curva ajusta sobre os pontos
      vindepseq <- seq(min(panel$vindep), max(panel$vindep), length.out=101)
      listparvar <- c(list(vindepseq), as.list(coef(n0)))
      names(listparvar)[1] <- varindep
      fx <- do.call("fmodele", listparvar)
      lines(vindepseq, fx, col=2)
      ## salva o ajuste numa lista
      if(is.null(panel$sbst)) { ajuste <<- n0 }
      if(!is.null(panel$sbst)) { ajuste[[panel$sbst]] <<- n0 }
    }
    panel
  }
  ##-----------------------------------------------------------------------------
  ## PORÇÃO COM OS CONTROLADORES.
  action <- nlr.draw
  ## abre o painel e as caixas de seleção
  if(is.null(subset)){
    nlr.panel <- rp.control(title="Ajuste", size=c(200, 200), model=model,
                            vdep=data[,vardep], vindep=data[,varindep], subset=NULL)
  }
  if(!is.null(subset)){
    nlr.panel <- rp.control(title="Ajuste", size=c(200, 200), model=model,
                            vdep=data[,vardep], vindep=data[,varindep],
                            subset=data[,subset])
    rp.listbox(nlr.panel, var=sbst, vals=levels(data[,subset]),
               rows=min(c(10, nlevels(data[,subset]))),
               title="subset", action=action)
  }
  ## abre os deslizadores para controlar o valor dos parâmetros
  if(any(names(start)=="th1"))
    rp.slider(panel=nlr.panel, var=th1,
              from=start[["th1"]]["from"],
              to=start[["th1"]]["to"],
              initval=start[["th1"]]["init"],
              showvalue=TRUE, action=action)
  if(any(names(start)=="th2"))
    rp.slider(panel=nlr.panel, var=th2,
              from=start[["th2"]]["from"],
              to=start[["th2"]]["to"],
              initval=start[["th2"]]["init"],
              showvalue=TRUE, action=action)
  if(any(names(start)=="th3"))
    rp.slider(panel=nlr.panel, var=th3,
              from=start[["th3"]]["from"],
              to=start[["th3"]]["to"],
              initval=start[["th3"]]["init"],
              showvalue=TRUE, action=action)
  if(any(names(start)=="th4"))
    rp.slider(panel=nlr.panel, var=th4,
              from=start[["th4"]]["from"],
              to=start[["th4"]]["to"],
              initval=start[["th4"]]["init"],
              showvalue=TRUE, action=action)
  if(any(names(start)=="th5"))
    rp.slider(panel=nlr.panel, var=th5,
              from=start[["th5"]]["from"],
              to=start[["th5"]]["to"],
              initval=start[["th5"]]["init"],
              showvalue=TRUE, action=action)
  rp.do(panel=nlr.panel, action=action)
  ## cria botão "adjust"
  rp.button(nlr.panel, action=nlsajust, title="adjust")
  invisible()
}

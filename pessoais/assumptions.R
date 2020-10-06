assumptions <- function(x,which=NULL,norm_type=c('shapiro','lillie'),homo_type=c('bartlet','levene'),round=2L){
	dados <- x$model
	residual <- residuals(x)
	normtype <- match.arg(norm_type)
	homotype <- match.arg(homo_type)

	resnorm <- switch(normtype,
			  shapiro = {
				  test <- shapiro.test(residual)
				  statistic <- test$statistic
				  pvalue <- test$p.value
				  shapiro <- round(c(statistic,pvalue),round)
			  },
			  lillie  = {
				  test <- nortest::lillie.test(residual)
				  statistic <- test$statistic
				  pvalue <- test$p.value
				  lillie <- round(c(statistic,pvalue),round)
			  })
        names(resnorm) <- c('Statistics','P-value')	
	reshomo <- switch(homotype,
			  bartlet = {
				  test <- with(dados,bartlett.test(residual,dados[[which]]))
				  statistic <- test$statistic
				  pvalue <- test$p.value
				  bartlet <- round(c(statistic,pvalue),round)
			  },
			  levene  = {
				  test <- with(dados,car::leveneTest(residual,dados[[which]]))
				  statistic <- test$'F value'[1]
				  pvalue <- test$'Pr(>F)'[1]
				  levene <- round(c(statistic,pvalue),round)
			  })
       names(reshomo) <- c('Statistics','P-value')
       result <- rbind(resnorm,reshomo)
       return(result)       
}


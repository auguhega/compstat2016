
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(Rcpp)
library(ggplot2)
library(dplyr)
library(vcd)
library(MASS)
library(plyr)

#Funcion tarea 1 
rExp <- function(nsim, lambda){
  return((-1/lambda)*log(1-runif(nsim)))
}

#Funcion tarea 2 

# int_trap <- function(fx, a, b){
#   N <- length(fx)
#   (b - a)/2*mean(fx[-N] + fx[-1])
# }

shinyServer(function(input, output) {

  
  fun1 <- reactive({
    texto <- paste("aux <- ", input$expresion1)
    eval(parse(text=texto))
    aux
  })
  
  fun2 <- reactive({
    switch(input$expresion2,
      "unif"= function(x) 1*(x>0 && x<1),
      "exp"= function(x) dexp(x)*(x>0),
      "norm" = function(x) dnorm(x)
    )
  })
  
  output$Grafica <- renderPlot({
    x <- seq(input$xmin, input$xmax, length.out=100)
    y1 <- sapply(x, fun1())
    y2 <- input$M*sapply(x, fun2())
    plot_limit = c(min(c(y1, y2)), max(c(y1, y2)))
    # tarea: investigar sapply, lapply, apply, tapply, mapply
    plot(x, y1, type="l", col="blue", main="Gráfica", ylim=plot_limit)
    lines(x, y2, col="red")
    legend("topright", c("f", "M*g"), col=c("blue", "red"), lty = 1)
  })
  # 
  
  simulaciones <- reactive({
    num_aceptados <- 0
    num_intentos <- 0
    sim_Y <- switch(input$expresion2,
      "unif"= function() runif(1),
      "exp"= function() rexp(1),
      "norm" = function() rnorm(1)
    )
    # print(sim_Y)
    valor_aceptados <- numeric(input$nsim)
    while(num_aceptados < input$nsim){
      Y <- sim_Y()
      U <- runif(1)
      if(Y >= input$xmin && Y<=input$xmax && U <= (fun1()(Y))/(input$M*(fun2()(Y)))){
        num_aceptados <- num_aceptados + 1
        valor_aceptados[num_aceptados] <- Y
      }
      num_intentos <- num_intentos + 1
    }
    # print(valor_aceptados)
    list(valor=valor_aceptados, tasa_exito=input$nsim/num_intentos)
  })
  
  output$tasa_exito <- renderText({
    simulaciones()$tasa_exito
  })
  
  output$hist_sim <- renderPlot({
    hist(simulaciones()$valor, main="Histograma de las simulaciones", breaks=input$nbins)
  })
#Tarea 1 
  
  res <- reactive(rExp(input$nsim_fi, input$lambda))
  
  output$tabla_datos <- renderDataTable(data.frame(res()))
  
  output$hist_sim_inv <- renderPlot(hist(res(), main="Histograma funcion inversa", breaks=input$nbins_i))
  
#Tarea 2
  
  funcionmc <- reactive({
    texto <- paste("aux <- ", input$funcionmc)
    eval(parse(text=texto))
    aux
  })
  
  
  mc.intervals <- function(Phi, N, X.dens=runif, alpha=al){
    
    results.list <- lapply(N, function(nsim_mc){
      
      X <- sapply(FUN=X.dens, nsim_mc) 
      PhiX <- sapply(X, Phi) 
      estim <- mean(PhiX) 
      S2 <- var(PhiX) 
      quant <- qnorm(alpha/2, lower.tail=FALSE) 
      int.upper <- estim + sqrt(S2/nsim_mc)*quant 
      int.lower <- estim - sqrt(S2/nsim_mc)*quant 
      return(data.frame(N=nsim_mc, Estimate=estim, LI=int.lower, UI=int.upper))
      # -------
    })
    #
    results.table <- ldply(results.list) 
    return(results.table)
  }
  
  
  
  output$grafica_sim<-renderPlot({
    
    nsim_mc<-input$nsim_mc
    al<-input$alpha
    Phi <- funcionmc()
    X.dens <- function(nsim_mc) runif(nsim_mc, input$a, input$b)
    N <- seq(from=input$nmin, to=input$nmax, by=100)
    data <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens,al)
    data
    
    g1<-ggplot(data, aes(x=N)) +
      geom_ribbon(aes(ymin=LI, ymax=UI), alpha=0.4) +
      geom_line(aes(y=Estimate), colour="red") 
    
    g1
    
  })
  
  
base<<-Boston[c("medv", "lstat" ,"crim", "indus", "black", "ptratio")]

output$table <- renderDataTable(base,
                                options = list(
                                  pageLength = 5,
                                  initComplete = I("function(settings, json) {alert('Done.');}")
                                )
)


Rcpp_In<- eventReactive(input$Simular,{
  colnames(base)<-c(1,2,3,4,5,6)
  y <- base[,c(input$dep)]
  cosas<<-c(input$checkGroup)
  x <- base[cosas]
  variables<<-length(cosas)
  
  #Definimos globalmente lad distribuciones a priori
  # beta_i ~ N(0,100)
  # sigma ~ Gamma(0,0.1)
  prior.beta <<- function(x) dnorm(x, 0, 1)
  prior.sigma <<- function(x) dexp(x, 0.001)
  
  source("mcmc_toolset.R")
  #Generamos la distribuci??n log posterior
  objdens(as.matrix(x), y, 1:3, 1)
  # 2) Proposal: caminata aleatoria en la misma dimensi??n que el n??mero de par??metros
  proposal(c(1,2,3), 1)
  
  # 3) METROPOLIS
  sourceCpp("BayesianMHLinReg.cpp")
  
  nsim <- input$N4
  init <- rep(0,ncol(x)+1) 
  sigma_init <- 1
  mh.samp <- MHBayesLinReg(nsim, init, sigma_init, objdens, proposal,
                           cbind(1,as.matrix(x)), y) 
  estims <<- mh.samp$theta
  estims_sigma <<- mh.samp$sigma
  str(mh.samp)
  
  
  #Estimadores puntuales 
  betahat <- apply(estims, 2, mean)
  betasd <- apply(estims, 2, sd)
  sigmahat <- mean(estims_sigma)
  sigmasd <- sd(estims_sigma)
  
  #Intervalos de probabilidad
  alpha <- 0.05
  intervals <- lapply(1:(ncol(x)+1), function(i){
    quantile(estims[ ,i], c(alpha/2, 1-alpha/2)) %>%
      t %>%
      as.data.frame
  }) %>%
    rbind_all
  interval_sigma <- quantile(estims_sigma, c(alpha/2, 1-alpha/2)) %>%
    t %>%
    as.data.frame
  
  Comparison <<- data.frame(betahat, betasd, intervals)
  colnames(Comparison) <- c('Estimate', 'sd', colnames(intervals))
  Comparison <- rbind(Comparison, c(Estimate=sigmahat, sd=sigmasd, interval_sigma))
  rownames(Comparison)[1:length(betahat)] <- paste0('beta',0:length(betahat))
  rownames(Comparison)[length(betahat)+1] <- 'sigma'
  out<-data.frame(betaHat=betahat)
  tabla<<-Comparison
  
})


output$value <- renderPrint({
  Rcpp_In()
  data.frame(tabla)
  
})
output$tablechain <- renderDataTable({
  Rcpp_In()
  data.frame(estims)
})

output$tablehat <- renderDataTable({
  Rcpp_In()
  data.frame(tabla)
})
output$ploH <- renderPlot({
  Rcpp_In()
  par(mfrow=c(3,2))
  for(j in 1:length(cosas)){
    hist(estims[ ,j], prob=TRUE)
  }
  hist(estims_sigma, prob=TRUE)
})  

output$boston1 <-renderPlot({
  pairs(Boston[c("medv", "lstat" ,"crim", "indus", "black", "ptratio")])      
})


B2 <- gather(MASS::Boston, BosVars, BosValues, crim:medv)
output$boston2 <-renderPlot({
  bostonPP2 <- ggplot(B2, aes(BosValues)) +
    geom_histogram() + xlab("") + ylab("") +
    facet_wrap(~ BosVars , scales = "free")
  bostonPP2  
})

output$boston3 <-renderPlot({
  with(MASS::Boston, {plot(density(medv)); rug(medv)})
})
##############

output$boston4 <-renderPlot({
  plot(Boston[c("lstat","medv")])
  abline(98.0054, 0.9528)
})


output$tableMH2 <- renderUI({
  Rcpp_In()
  texto<-"$$Y="
  texto=paste(texto,Comparison$betahat[1])
  num<-variables
  for (i in 1:num){
    signo='+'
    if(Comparison$betahat[i+1]<0){signo=''}
    texto=paste(texto,signo,Comparison$betahat[i+1],"X_{",i,"}")
  }
  return (withMathJax(paste(texto,"$$")))
})

output$tableMH <- renderUI({
  Rcpp_In()
  texto<-"$$Y=\\beta_{0}"
  num<-variables
  for (i in 1:num){
    texto=paste(texto,"+\\beta_{",i,"} X_{",i,"}")
  }
  return (withMathJax(paste(texto,"$$")))
})
output$tableHat <- renderTable({
  
  data.frame(Rcpp_In())
})   

     
  
})

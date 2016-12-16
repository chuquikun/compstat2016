#Layout server

library(shiny)
library(shinydashboard)
library(ggplot2)
require(plyr)
library(DT)
library(Rcpp)
sourceCpp('rcpp_examples.cpp')
library(invgamma)
#transit <- read.csv("\Transit_demand.csv",header=TRUE,sep=",")

shinyserver <- function(input, output) {
##############################################    Tarea 1     ##########################################################################################  
 
  rExp <- function(simu, lamb){
    lamb <- (input$lambda)
    
    return((-1/lamb)*log(1-runif(simu)))
  }
  
   output$plot1 <- renderPlot({
    lamb <- (input$lambda)
    simu <- (input$Simulaciones)
  
   
    dat <- data.frame(Value=rExp(simu, lamb))
    ggplot(dat, aes(x=Value)) +
      geom_histogram(aes(y=..density..), binwidth= .2, colour="black", fill="white") +
      stat_function(fun = function(x) lamb*exp(-lamb*x),colour = "blue")
  })
  
  output$value <- renderPrint({ 
    lamb <- (input$lambda)
    simu <- (input$Simulaciones)
    alp<-(input$alp)
    set.seed(166940)

    print(ks.test(rExp(simu, lamb), pexp, lamb))

    pv<-ks.test(rExp(simu, lamb), pexp, lamb)$p.value
        
    if(pv<=alp){
      print( paste("Los datos no se comportan exponencial con lambda:", lamb, "y alpha:",alp))
    }else{
      print(paste("Es un buen supuesto que los datos se comportan de manera exponencial con lambda:",lamb, "y alpha:",alp))
    }
    })

##############################################    Tarea 2     #########################################################################################  

    fun1 <- reactive({
      texto <- paste("aux <- ", input$expresion1)
      eval(parse(text=texto))
      aux
  })
  
 
 
  
  output$Integracion <- renderPrint({
    
    ####Parametros introducidos por el usuarios
    step<-input$NN
    diezstep<- 10*step
    Phi<-fun1()
    X.dens <- function(nsim) runif(nsim, input$N1,input$N2)
    N <- seq(step,diezstep,step)
    al<-input$alpha
    k<-input$N2-input$N1
    
    
    ####Formula para el calculo de la integral
    mc.intervals <- function(Phi, N, X.dens,al=.05,k=1){
      set.seed(166940)
      results.list <- lapply(N, function(nsim){
        X <- sapply(FUN=X.dens, nsim) # N samples of the density of X
        PhiX <- sapply(X, Phi) # Evaluate phi at each X_i
        estim <- mean(PhiX) # Estimate of int_a^b \phi(x)f(x)df=E[phi(X_i)]
        S2 <- var(PhiX) # Estimate of the variance of phi(X_i)
        quant <- qnorm(al/2, lower.tail=FALSE) # Right quantile for alpha/2
        int.upper <- k*estim + sqrt(S2/nsim)*quant # Upper confidence interval
        int.lower <- k*estim - sqrt(S2/nsim)*quant # Lower confidence interval
        return(data.frame(Muestra=nsim, Estimado=k*estim, LI=int.lower, UI=int.upper))
        # -------
      })
      #
      results.table <- ldply(results.list) # Assembles list in data.frame
      return(results.table)
    }
    
    
    ####Caculo de la funcion
    data <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens)[1,]
    
    
    print("El resultado de la integral se presenta en el campo Estimado")
    print(data)
    
    })
  
  ####Grafico de la funcion
  output$plot2<-renderPlot({
  
    plot(fun1(),input$N1,input$N2,type='l',col="blue",xlab="x",ylab="f(x)")
     
    
    })
  
  # ####Grafico de la funcion de los intervalos de confianza
   output$plot3<-renderPlot({
     
     ####Parametros introducidos por el usuarios
     step<-input$NN
     diezstep<- 10*step
     Phi<-fun1()
     X.dens <- function(nsim) runif(nsim, input$N1,input$N2)
     N <- seq(step,diezstep,step)
     al<-input$alpha
     k<-input$N2-input$N1
     
     
     ####Formula para el calculo de la integral
     mc.intervals <- function(Phi, N, X.dens,al=.05,k=1){
       set.seed(166940)
       results.list <- lapply(N, function(nsim){
         X <- sapply(FUN=X.dens, nsim) # N samples of the density of X
         PhiX <- sapply(X, Phi) # Evaluate phi at each X_i
         estim <- mean(PhiX) # Estimate of int_a^b \phi(x)f(x)df=E[phi(X_i)]
         S2 <- var(PhiX) # Estimate of the variance of phi(X_i)
         quant <- qnorm(al/2, lower.tail=FALSE) # Right quantile for alpha/2
         int.upper <- k*estim + sqrt(S2/nsim)*quant # Upper confidence interval
         int.lower <- k*estim - sqrt(S2/nsim)*quant # Lower confidence interval
         return(data.frame(Muestra=nsim, Estimado=k*estim, LI=int.lower, UI=int.upper))
         # -------
       })
       #
       results.table <- ldply(results.list) # Assembles list in data.frame
       return(results.table)
     }
     
     
     ####Caculo de la funcion
     datafull <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens)
     
   ggplot(datafull, aes(x=Muestra)) +
   geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey") +
   geom_line(aes(y=Estimado), colour="blue") 
   })

##############################################    Tarea 3     #########################################################################################     
   
   

   MatT <- reactive({
     inFile<-input$archi
     Matriz <- read.csv(inFile$datapath, header=FALSE, sep=",")
     Matriz<-as.matrix(Matriz)
     dimnames(Matriz)<-list(seq(1,dim(Matriz)[1],1),seq(1,dim(Matriz)[1],1))
     Matriz
     })


   
   output$corrida <- renderPrint({
 
     lamat<-MatT()
     camino<-markovchain_trajectory(init_state=input$inicio, n_transitions=input$trans, trans_mat=lamat)
     print(camino)

   })
   
   output$tabla<-renderDataTable(matriz<-MatT()
                                 )
   
##############################################    Tarea 4    #########################################################################################     

   
   ArCSV <- reactive({
     inFile <-input$archivo
     transit <- read.csv(inFile$datapath, header=TRUE, sep=",")
     #Matriz2 <-as.matrix(Matriz2)
     #dimnames(Matriz)<-list(seq(1,dim(Matriz)[1],1),seq(1,dim(Matriz)[1],1))
     return (transit)
   })
   
   output$mytrans<-renderDataTable(ArCSV())
                                 
   
   #Base de datos para la regresion   
   #output$mytrans <- DT::renderDataTable({
   # DT::datatable(transit)
   #})
   
   #Scatterplot
   
   output$dispersion<-renderPlot({
     
     a=3
     b=6
     for (i in(2:6)){
       if (input$variableX== i) {a=i} 
       if (input$variableY== i) {b=i} 
     }
     
     datafrm <- ArCSV()
     
     #equis<-transit[,a]
     equis<-datafrm[,a] 
     #ye<-transit[,b]
     ye<-datafrm[,b]
     sp<-data.frame(equis,ye)
     
     
     ggplot(sp, aes(x=equis, y=ye)) +
       geom_point(shape=1) +  
       geom_smooth(method = lm) #De repente dejo de pintar la recta de la regresion
   })
   
#Aprioris
   
   output$aprioris <- renderPrint({
     
     a=3
     b=6
     for (i in(2:6)){
       if (input$variableX== i) {a=i} 
       if (input$variableY== i) {b=i} 
     }
     
     datafrm <- ArCSV()
     
     
     #equis<-c(transit[,a])
     equis<-c(datafrm[,a])
     #ye<-c(transit[,b])
     ye<-c(datafrm[,b])
     sp<-data.frame(equis,ye)
     splm<-lm(ye~equis,data=sp)
     summary_splm<-summary(splm)
     betas<-coefficients(summary_splm)
     
     
     print(paste(" b0~Normal(",round(betas[1,1],digits=2), ",",round(betas[1,2],digits=2),")"))
     print(paste(" b1~Normal(",round(betas[2,1],digits=2), ",",round(betas[2,2],digits=2),")"))
     print(paste(" sigma^2~InvGamma(",27/2, ",",round(25*summary_splm$sigma,digits=2),")"))
     
     
   })
   
 #graficas aprioris
   
  
   output$apriori1 <- renderPlot({
     a=3
     b=6
     for (i in(2:6)){
       if (input$variableX== i) {a=i} 
       if (input$variableY== i) {b=i} 
     }
     
     datafrm <- ArCSV()
     
     #equis<-c(transit[,a])
     equis<-c(datafrm[,a])
     #ye<-c(transit[,b])
     ye<-c(datafrm[,b])
     sp<-data.frame(equis,ye)
     splm<-lm(ye~equis,data=sp)
     summary_splm<-summary(splm)
     betas<-coefficients(summary_splm)
     
     x <- seq(-25, 25, length=100)
     plot(x, dnorm(x,round(betas[1,1],digits=2),round(betas[1,2],digits=2)), type="l", lty=2, xlab="x value",
          ylab="Density", main= paste("Apriori b0~Normal(",round(betas[1,1],digits=2), ",",round(betas[1,2],digits=2),")"))
     
   })
   output$apriori2 <- renderPlot({
     a=3
     b=6
     for (i in(2:6)){
       if (input$variableX== i) {a=i} 
       if (input$variableY== i) {b=i} 
     }
     
     datafrm <- ArCSV()
     
     #equis<-c(transit[,a])
     equis<-c(datafrm[,a])
     #ye<-c(transit[,b])
     ye<-c(datafrm[,b])
     
     sp<-data.frame(equis,ye)
     splm<-lm(ye~equis,data=sp)
     summary_splm<-summary(splm)
     betas<-coefficients(summary_splm)
     
     x <- seq(-25, 25, length=100)
     plot(x, dnorm(x,round(betas[2,1],digits=2),round(betas[2,2],digits=2)), type="l", lty=2, xlab="x value",
          ylab="Density", main= paste("Apriori b0~Normal(",round(betas[2,1],digits=2), ",",round(betas[2,2],digits=2),")"))
     
   })
   output$apriori3 <- renderPlot({
     a=3
     b=6
     for (i in(2:6)){
       if (input$variableX== i) {a=i} 
       if (input$variableY== i) {b=i} 
     }
     
     datafrm <- ArCSV()
     
     #equis<-c(transit[,a])
     equis<-c(datafrm[,a])
     #ye<-c(transit[,b])
     ye<-c(datafrm[,b])
     
     sp<-data.frame(equis,ye)
     splm<-lm(ye~equis,data=sp)
     summary_splm<-summary(splm)
     betas<-coefficients(summary_splm)
     
     x <- seq(-25, 25, length=100)
     plot(x, dinvgamma(x,13.5,round(25*summary_splm$sigma,digits=2)), type="l", lty=2, xlab="x value",
          ylab="Density", main=paste(" sigma^2~InvGamma(",27/2, ",",round(25*summary_splm$sigma,digits=2),")"))
   })
   
################################### Tarea 5 #######################################   
   
   selecciones<-reactive({
     
     a=3
     b=6
     for (i in(2:6)){
       if (input$variableX== i) {a=i} 
       if (input$variableY== i) {b=i} 
     }
     
     datafrm <- ArCSV()
     
     #equis<-c(transit[,a])
     equis<-c(datafrm[,a])
     #ye<-c(transit[,b])
     ye<-c(datafrm[,b])
     
     list('x'=equis,'y'=ye)  
     
   })
   

   nreg <- reactive({
     
     sp<-data.frame(selecciones()$x,selecciones()$y)
     splm<-lm(selecciones()$y~selecciones()$x,data=sp)
     summary_splm<-summary(splm)
     betas<-coefficients(summary_splm)
     list('betas' = betas, 'summary' = summary_splm)
   })
   
  
     
     likelihood <- function(param){
       b1= param[1]
       b0 = param[2]
       sigma2 = param[3]
       
       pred = b1*selecciones()$x + b0
       singlelikelihoods = dnorm(selecciones()$y, mean = pred, sd = sigma2**.5, log = T)
       sumll = sum(singlelikelihoods)
       return(sumll)   
     }
     
     prior <- function(param){
       b1 = param[1]
       b0 = param[2]
       sigma2 = param[3]
       b1prior = dnorm(b1, mean=round(nreg()$betas[1,1],digits=2), sd=round(nreg()$betas[1,2]**.5,digits=2), log = T)
       b0prior = dnorm(b0, mean=round(nreg()$betas[2,1],digits=2), sd=round(nreg()$betas[2,2]**.5,digits=2), log = T)
       sigma2prior = dinvgamma(sigma2,14,round(25*nreg()$summary$sigma,digits=2),log = T)
       return(b1prior+b0prior+sigma2prior)
     }
     
     posterior <- function(param){
       return (likelihood(param) + prior(param))
     }
     
     ######## Metropolis algorithm ################
     
     proposalfunction <- function(param){
       return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
     }
     
     run_metropolis_MCMC <- function(startvalue, iterations){
       chain = array(dim = c(iterations+1,3))
       chain[1,] = startvalue
       for (i in 1:iterations){
         proposal = proposalfunction(chain[i,])
         
         logprobab =posterior(proposal) - posterior(chain[i,])
         if (log(runif(1)) <= logprobab){
           chain[i+1,] = proposal
         }else{
           chain[i+1,] = chain[i,]
         }
       }
       return(chain)
     }
     
     #correle<-eventReactive(input$button,{ 
     resmcmc<-eventReactive(input$button,{
       
       
     startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
     chain = run_metropolis_MCMC(startvalue, input$numcad)
     chain=data.frame(b1=chain[,1],b0=chain[,2],s2=chain[,3])
      
     if(input$longcad>1){ 
     for (i in 2:input$longcad){
       
       startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
       chain2 = run_metropolis_MCMC(startvalue, input$numcad)
       chain2=data.frame(b1=chain2[,1],b0=chain2[,2],s2=chain2[,3])
       chain=cbind(chain,chain2)
       
     }}
     
      return(chain)
     })
     
     
     #for (i in 2:input$longcad){
     # startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
     #chain = run_metropolis_MCMC(startvalue, input$numcad)
     #newchain<-data.frame(b1=chain[,1],b0=chain[,2],s2=chain[,3])
     #lachain<-cbind(lachain,newchain)
     #}
     
     output$cadenas<-renderDataTable({ 
      resmcmc()
   })   
   
  
   
   
   output$histos<-renderPlot({
     
     burnIn = input$numcad*.20
     acceptance = 1-mean(duplicated(resmcmc()[-(1:burnIn),]))
     par(mfrow = c(2,3))
     hist(resmcmc()[-(1:burnIn),1],nclass=30,  main="Posterior of b1", xlab="Parametro" )
     abline(v = mean(resmcmc()[-(1:burnIn),1]))
     hist(resmcmc()[-(1:burnIn),2],nclass=30, main="Posterior of b0", xlab="Parametro")
     abline(v = mean(resmcmc()[-(1:burnIn),2]))
     hist(resmcmc()[-(1:burnIn),3],nclass=30, main="Posterior of sigma^2", xlab="Parametro")
     abline(v = mean(resmcmc()[-(1:burnIn),3]) )
     plot(resmcmc()[-(1:burnIn),1], type = "l", xlab="Iteraciones" , main = "Chain values of b1" )
     plot(resmcmc()[-(1:burnIn),2], type = "l", xlab="Iteraciones" , main = "Chain values of b0")
     plot(resmcmc()[-(1:burnIn),3], type = "l", xlab="Iteraciones" , main = "Chain values of sigma^2")
     
        })
   
   output$distos<-renderPlot({
     
     burnIn = input$numcad*.20
  
     par(mfrow = c(1,3))
     
     d1 <- density(resmcmc()[-(1:burnIn),1])
     d2 <- density(resmcmc()[-(1:burnIn),2])
     d3 <- density(resmcmc()[-(1:burnIn),3])
     
     plot(d1,main = "Posteriori of b1")
     plot(d2,main = "Posteriori of b0")
     plot(d3,main = "Posteriori of sigma^2")
     
     
     
     
   })
   
}




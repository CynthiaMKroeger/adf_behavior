ImpLMEModelSummary <- function( ModelSummaryAll = ModelSummaryAll , 
                                m = m )
{
  
  m <- m
  
  p <- length( ModelSummaryAll[[1]]$ModelSummary$coefficients$fixed )
  Q <- matrix( NA , m , p )
  U <- matrix( NA , m , p )
  
  p_emm <- dim( ModelSummaryAll[[1]]$EmmSummary )[1]
  Q_emm <- matrix( NA , m , p_emm )
  U_emm <- matrix( NA , m , p_emm )
  
  p_cont <- dim( ModelSummaryAll[[1]]$ContSummary )[1]
  Q_cont <- matrix( NA , m , p_cont )
  U_cont <- matrix( NA , m , p_cont )
  
  
  for( i in 1:m )
  {
    Q[i,] <- ModelSummaryAll[[i]]$ModelSummary$coefficients$fixed
    U[i,] <- diag(ModelSummaryAll[[i]]$ModelSummary$varFix)
    
    Q_emm[i,] <- ModelSummaryAll[[i]]$EmmSummary$emmean
    U_emm[i,] <- (ModelSummaryAll[[i]]$EmmSummary$SE)^2
    
    Q_cont[i,] <- ModelSummaryAll[[i]]$ContSummary$estimate
    U_cont[i,] <- (ModelSummaryAll[[i]]$ContSummary$SE)^2
  }

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LME Summary
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  Q_bar <- apply( Q , 2 , mean )
  Q_bar  # "est"
  
  U_bar <- apply( U , 2 , mean )
  B <- diag( var( Q ) )
  Tee <- U_bar + (1+(1/m)) * B
  SE <- sqrt(Tee)  # "se"
  
  t <- Q_bar / sqrt(Tee)  # "t"
  
  nu <- (m-1) * ( 1 + U_bar / ((1+1/m)*B) ) ^2
  nu_com <- dim( Data[Data$.imp==1,] )[1] - p
  nu_obs <- ( 1 - (1+(1/m))*B / Tee ) * ( (nu_com+1)/(nu_com+3)*(nu_com) )
  nu_star <- 1 / ( 1/nu + 1/nu_obs )
  DF <- nu_star  # "df"
  
  p.value <- round( 2 * ( 1 - pt( abs(t) , df=nu_star ) ) , 5 )
  p.value  # "Pr(>|t|)"
  
  # summary
  ImpModelSummary <- data.frame( Q_bar , SE , t , DF , p.value )
  rownames(ImpModelSummary) <- names( ModelSummaryAll[[1]]$ModelSummary$coefficients$fixed )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # EMMs Summary
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Q_emm_bar <- apply( Q_emm , 2 , mean )
  # Q_emm_bar  # "est"
  
  U_emm_bar <- apply( U_emm , 2 , mean )
  B_emm <- diag( var( Q_emm ) )
  Tee_emm <- U_emm_bar + (1+(1/m)) * B_emm
  SE_emm <- sqrt(Tee_emm)  # "se"
  
  # t_emm <- Q_emm_bar / sqrt(Tee_emm)  # "t"
  # 
  # nu_emm <- (m-1) * ( 1 + U_emm_bar / ((1+1/m)*B_emm) ) ^2
  # DF_emm <- nu_emm  # df
  # 
  # p.value_emm <- round( 2 * ( 1 - pt( abs(t_emm) , df=nu_emm ) ) , 5 )
  # p.value_cont  # "Pr(>|t|)"
  
  # summary
  ImpEmmSummary <- data.frame( ModelSummaryAll[[1]]$EmmSummary[colnames(ModelSummaryAll[[1]]$EmmSummary)[1]] ,
                               ModelSummaryAll[[1]]$EmmSummary[colnames(ModelSummaryAll[[1]]$EmmSummary)[2]] ,
                               Q_emm_bar , SE_emm )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Contrast Summary
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  Q_cont_bar <- apply( Q_cont , 2 , mean )
  # Q_cont_bar  # "est"
  
  U_cont_bar <- apply( U_cont , 2 , mean )
  B_cont <- diag( var( Q_cont ) )
  Tee_cont <- U_cont_bar + (1+(1/m)) * B_cont
  SE_cont <- sqrt(Tee_cont)  # "se"
  
  t_cont <- Q_cont_bar / sqrt(Tee_cont)  # "t"
  
  nu_cont <- (m-1) * ( 1 + U_cont_bar / ((1+1/m)*B_cont) ) ^2
  DF_cont <- nu_cont  # df
  
  p.value_cont <- round( 2 * ( 1 - pt( abs(t_cont) , df=nu_cont ) ) , 5 )
  # p.value_cont  # "Pr(>|t|)"
  
  # summary
  ImpContSummary <- data.frame( ModelSummaryAll[[1]]$ContSummary[colnames(ModelSummaryAll[[1]]$ContSummary)[1]] ,
                                ModelSummaryAll[[1]]$ContSummary[colnames(ModelSummaryAll[[1]]$ContSummary)[2]] ,
                                Q_cont_bar , SE_cont , t_cont , DF_cont , p.value_cont )
  
  
  return( list( Fixed = ModelSummaryAll[[1]]$Fixed ,
                ImpModelSummary = ImpModelSummary ,
                ImpEmmSummary = ImpEmmSummary ,
                ImpContSummary = ImpContSummary ))
  
}
LMEModelSummary <- function( Fixed = Fixed ,
                             Random = Random ,
                             Data = Data ,
                             Specs = Specs )
{
  #~~~~~~~~~~
  # model
  #~~~~~~~~~~
  Model <- lme( fixed = Fixed ,
                random = Random ,
                data = Data )
  ModelSummary <- summary( Model )
  
  # estimated marginal mean
  Emm <- emmeans( Model , specs = Specs )
  EmmSummary <- summary( Emm )
  
  # contrast
  Cont <- contrast( Emm , method='pairwise' )
  ContSummary <- summary( Cont )
  
  return( list( Fixed = Fixed ,
                ModelSummary = ModelSummary , 
                EmmSummary = EmmSummary ,
                ContSummary = ContSummary ) )
}

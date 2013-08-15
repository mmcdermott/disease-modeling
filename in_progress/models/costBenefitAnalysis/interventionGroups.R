source('interventionAnalyzer.R')
#Group by type IMPORTANT: Reset all intervention costs to default values in interventionConfig.R before running
# for(intervention in redEnLTBI_Interventions) {
  # intConfig <- interventionConfig(intervention)
  # costs     <- intConfig$costs
  # params    <- intConfig$params
  # interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]],
                    # params[["incLTBI"]])
  # write.csv(interData, paste(c(intFilePrefix,intervention,intFileSuffix),
                             # collapse=""))
# }
# for(intervention in incLTBItrmt_Interventions) {
  # intConfig <- interventionConfig(intervention)
  # costs     <- intConfig$costs
  # params    <- intConfig$params
  # interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]],
                    # params[["incLTBI"]])
  # write.csv(interData, paste(c(intFilePrefix,intervention,intFileSuffix),
                             # collapse=""))
# }


#Cost Vary
for(x in 0:9){
  for(intervention in redEnLTBI_Interventions) {
    intConfig <- interventionConfig(intervention,x)
    costs     <- intConfig$costs
    params    <- intConfig$params
    interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]],
                    params[["incLTBI"]])
    write.csv(interData, paste(c(intFilePrefix,intervention,x,intFileSuffix),
                             collapse=""))
  }
  # for(intervention in incLTBItrmt_Interventions) {
    # intConfig <- interventionConfig(intervention,x)
    # costs     <- intConfig$costs
    # params    <- intConfig$params
    # interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]],
                    # params[["incLTBI"]])
    # write.csv(interData, paste(c(intFilePrefix,intervention,x,intFileSuffix),
                             # collapse=""))
  # }
}

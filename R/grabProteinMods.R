grabProteinMods <- function(loc = 'bannerModules.csv'){
  synapseClient::synapseLogin()
  synIds<-c('BLSAsynId'='syn7421789',
    'ACTsynId'='syn7417624')
  proteinMods<-rSynapseUtilities::loadDelimIntoList(synIds)
  proteinMods[[1]] <- proteinMods[[1]][,c(1,3)]
  proteinMods[[2]] <- proteinMods[[2]][,c(2,3)]
  proteinMods <- lapply(proteinMods,
                        function(x) {colnames(x) <- c('Gene','Module');return(x)})
  proteinMods <- lapply(proteinMods,
                        function(x){x <- data.frame(x,stringsAsFactors = F);return(x)})
  proteinMods$syn7421789$Study <- rep('BLSA',nrow(proteinMods$syn7421789))
  proteinMods$syn7417624$Study <- rep('ACT',nrow(proteinMods$syn7417624))

  proteinMods <- do.call(rbind,proteinMods)


  #####add in banner modules to pipeline
  bannerMods <- data.table::fread(loc,data.table=F)
  bannerMods$GeneNew <- sapply(bannerMods$Gene,function(x){strsplit(x,'\\|')[[1]][1]})
  bannerMods <- bannerMods[,c(3,2)]
  bannerMods$Study <- rep('Banner',nrow(bannerMods))
  colnames(bannerMods)[1] <- 'Gene'
  proteinMods <- rbind(proteinMods,bannerMods)
  return(proteinMods)
}

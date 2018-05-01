get_eigengenes_tissue <- function(synId = 'syn10309369',tissue = 'DLPFC'){
  #aggMods <- rSynapseUtilities::loadFullTable(synId)
  synapseClient::synapseLogin()
  mods <- synapseClient::synTableQuery(paste0("select * from ",synId," where brainRegion = \'",tissue,'\''))@values
  geneExpressionForAnalysis <- AMPAD::pullExpressionAndPhenoWinsorized()
  names(geneExpressionForAnalysis) <- c('TCX','CBE','DLPFC','FP','STG','PHG','IFG')
  computeEigengene <- function(br,geneExp,moduleDefinitions){

    geneExp <- geneExp[[br]]

    #get modules
    #mods <- dplyr::filter(moduleDefinitions,brainRegion==br)
    #convert modules into list of genes
    modsDefs <- lapply(unique(mods$ModuleNameFull),
                       utilityFunctions::listify,
                       mods$GeneID,
                       mods$ModuleNameFull)

    names(modsDefs) <- unique(mods$ModuleNameFull)

    internal <- function(mod,modsDefs,geneExp){
      geneExpMod <- dplyr::select(geneExp,modsDefs[[mod]])
      geneExpMod <- scale(geneExpMod)
      foo <- svd(geneExpMod)
      eigenGenes <- as.matrix(foo$u[,1])
      colnames(eigenGenes) <- paste0('pc',1)
      rownames(eigenGenes) <- geneExp$aSampleId
      #res <- cor(eigenGenes,geneExpMod)
      return(eigenGenes)
    }

    full_res<-lapply(names(modsDefs),internal,modsDefs,geneExp)
    names(full_res) <- names(modsDefs)
    return(full_res)
  }

  fullList<-computeEigengene(tissue,geneExpressionForAnalysis,mods)
  #fullList<-lapply(names(geneExpressionForAnalysis)[keep],computeEigengene,geneExpressionForAnalysis,aggMods)
  #names(fullList) <- names(geneExpressionForAnalysis)
  return(fullList)
}

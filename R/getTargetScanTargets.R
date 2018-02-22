getTargetScanTargets <- function(miRs = c('miR-484/3155','miR-197-3p','miR-197-5p/3132')){
  targetScanDf <- data.table::fread('Nonconserved_Family_Info.txt',
                                    data.table=F)
  targetScanDf <- dplyr::filter(targetScanDf,`Species ID` == 9606)
  tsDfReduced <- dplyr::filter(targetScanDf,`miR Family` %in% miRs)
  return(tsDfReduced)
}

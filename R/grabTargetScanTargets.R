grabTargetScanTargets <- function(){
  system('curl -O http://www.targetscan.org/vert_71/vert_71_data_download/Nonconserved_Family_Info.txt.zip')
  system('unzip Nonconserved_Family_Info.txt.zip')
}

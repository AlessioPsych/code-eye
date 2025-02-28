rm(list=ls())

# toolbox and functions
generalPurposeDir <- Sys.getenv( x='AFNI_TOOLBOXDIRGENERALPURPOSE' )
afniInstallDir <- Sys.getenv( x='AFNI_INSTALLDIR' ) 
source( sprintf('%s/scaleData.R', generalPurposeDir) )
source( sprintf('%s/AFNIio.R', afniInstallDir ) )
library( pracma )
library( abind )
library( circular )

# folders 
mainFolder <- '/home/fracasso/Data/analyse/Project0226/KastnerModel/staging_area_Kastner'
surfaceFolder <- '/home/fracasso/Data/analyse/Project0226/KastnerModel/staging_area_Kastner/anatomies_KastnerClassic_Freesurfer/surfaceAtlases/suma_MNI152_2009'
anatomyDir <- sprintf( '%s/anatomies_KastnerClassic_Freesurfer', mainFolder )
testSubjectDir <- '/home/fracasso/Data/analyse/Project0226/KastnerModel/staging_area_Kastner/anatomies_KastnerClassic_Freesurfer/ASM16_ANATOMY/FreeSeg_result/SUMA/kastnerClassicModelsSurfaces1D'

setwd( testSubjectDir )
rawFilenames <- list.files( path=getwd(), recursive = FALSE, full.names = FALSE )
testSubject <- 'ASM16'
cleanFilenames <- rep('a',length(rawFilenames))
for ( nModeName in 1:length(rawFilenames) ) {
  testFilename <- rawFilenames[ nModeName ]
  cleanFilenames[ nModeName ] <- strsplit( testFilename, testSubject )[[1]][2] 
}

runCodeFlag <- 1

Sys.setenv(OMP_NUM_THREADS='6')
Sys.getenv('OMP_NUM_THREADS')

#### create outputFolder folder, kastnerClassic ####
dirToCheck <- sprintf('%s/kastnerClassicAverageModelsSurfaces1D', surfaceFolder )
if ( dir.exists( dirToCheck ) ) {
  instr <- sprintf( 'rm -R %s', dirToCheck ) 
  print( instr )
  if (runCodeFlag==1) { system( instr ) }  
}  
if ( !dir.exists( dirToCheck ) ) {
  instr <- sprintf('mkdir %s', dirToCheck)
  print( instr )
  if (runCodeFlag==1) { system( instr ) }  
}         

for ( nAverages in 1:length( cleanFilenames ) ) { #length( cleanFilenames ) #nAverages <- 1
  
  dirTest <- list.files( path=anatomyDir, pattern=cleanFilenames[ nAverages ], all.files = TRUE, recursive = TRUE )
  nameCheckArray <- rep(0,length(dirTest))
  for (counter in 1:length(dirTest)) { #for loop to check that all file names are correct, do not include strings with 'average'
    if ( !is.null( strfind( dirTest[counter], 'average'  ) ) ) { nameCheckArray[counter] <- 1 }
  }
  dirTest <- dirTest[ nameCheckArray==0 ] # filter dirTest with only correct files
  
  setwd( anatomyDir )
  print( getwd() )
  print( dirTest[ 1 ] )
  fileTemp <- read.table( dirTest[ 1 ], as.is = TRUE )
  
  #fileStore <- array( 0, dim( fileTemp ) )
  fileStoreLoop <- array( 0, c( dim( fileTemp ), length( dirTest ) ) )
  for ( nFiles in 1:length( dirTest ) ) { #nFiles <- 1
    setwd( anatomyDir )
    print( getwd() )
    print( dirTest[ nFiles ] )
    fileTemp <- read.table( dirTest[ nFiles ], as.is = TRUE )
    #fileStore <- fileStore + fileTemp
    fileStoreLoop[,,nFiles] <- as.matrix( fileTemp )
  }
  
  fileStore <- array( 0, c( dim( fileTemp ) ) )
  phaseInformationIndex <- ifelse( is.null( strfind( cleanFilenames[nAverages], 'phase' ) ), 18, 7 )
  # if the data ( cleanFilenames[nAverages] ) is a model, then phase info is stored in column 18,
  # otherwise, if it is phase encoded (hence, looking for 'phase' in the filename), then pahse information is stored in
  # column 7
  for ( nCols in 1:dim(fileStoreLoop)[2] ) {
    print( sprintf('computing average column %d out of %d...', nCols, dim(fileStoreLoop)[2] ) )
    if (nCols!=phaseInformationIndex) { #7 # where preferred phase information is stored (saccade direction)
      fileStore[,nCols] <- apply( fileStoreLoop[,nCols,], 1, mean )
    } else {
      suppressWarnings( fileStore[,nCols] <- apply( fileStoreLoop[,nCols,], 1, mean.circular ) )
    }
  }
  #fileStore <- fileStore/length( dirTest )
  fileNameOutput <- paste( 'average', cleanFilenames[ nAverages ], sep = '' )

  write.table( x=round(fileStore,4), file=sprintf('%s/kastnerClassicAverageModelsSurfaces1D/%s', surfaceFolder, fileNameOutput ), col.names = FALSE, row.names = FALSE )
  
  rm( fileStoreLoop )
  rm( fileStore )
  gc()
  
}





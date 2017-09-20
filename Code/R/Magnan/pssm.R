loadPSSM <-
  function(pssmFolder) {
    cat(as.character(Sys.time()),">> Reading PSSM files ...\n");
    
    mapPSSM = new.env();
    
    # get the list of file names
    lstFiles = dir(pssmFolder);
    
    for (fileName in lstFiles) {
      splits = strsplit(toString(fileName),"\\.")[[1]]
      
      id = splits[1];
      
      filePath = paste(pssmFolder, "/", fileName, sep = "");
      pssm = read.csv(filePath);
      
      assign(id, pssm, mapPSSM);
    }
    
    cat(as.character(Sys.time()),">> Done ...\n");
  
    return(mapPSSM)
  }
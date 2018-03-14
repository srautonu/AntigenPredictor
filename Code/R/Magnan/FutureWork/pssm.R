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

normalizePSSM <-
  function(rawPSSM) {
    rawPSSMmapFile = "mapPSSM_Raw.rds";
    
    cat(as.character(Sys.time()),">> Reading Raw PSSM map from:", rawPSSMmapFile, "...\n");
    mapPSSM = readRDS(rawPSSMmapFile)
    cat(as.character(Sys.time()),">> Done ...\n");
    
    #
    # Normalize the pssmMap through appropriate transformation
    # Here we are using a sigmoid function (range: [0,1])
    #
    cat(as.character(Sys.time()),">> Normalizing PSSM using sigmoid function ...\n");
    for (protId in ls(mapPSSM)) {
      curPssm = get(protId, envir = mapPSSM);
      curPssm = 1 / (1 + exp(-curPssm));
      assign(protId, curPssm, envir = mapPSSM)
    }
    cat(as.character(Sys.time()),">> Done.\n");
    
    return(mapPSSM)
  }
#' Illustration of Featurization
#'
#' This function takes dataset and extracts PSSM based features.
#'
#' @param Ids Identifiers of the sequences. Used to extract PSSM from pssmMap
#' @param sequences provided as dataframe
#' @param labels class label of each data. Provided as a column vector
#' @param alphabet is the list of aminoacids
#'        c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "Y")
#' @param nGramOrder Highest value of n in n-grams feature extraction technique
#' @param nGDipOrder Highest value of n in n-Gapped-Dipeptide (nGDip) feature extraction technique
#' @return a featurized dataframe
#' @export
featurizationWithPSSM <-
  function(Ids, sequences, labels, pssmMap, alphabet, nGramOrder, nGDipOrder) {
    
    features = data.frame(1:length(sequences))
    # a dummy column got created. Let us name it. We will
    # delete this column at the end
    colnames(features)[length(features)] = "Serial"
    
    nGramCount = 0;
    nGDipCount = 0;
    
    #
    # Create the necessary columns in the feature vector
    #
    for (order in 1:nGramOrder) {
      temp = expand.grid(rep(list(alphabet), order))
      temp = do.call(paste0, c("C_0_", temp))
      features[temp] = double(nrow(features));
    }

    for (i in 1:nrow(features)) {
        strSeq = strsplit(toString(sequences[i]), "")[[1]];
        pssm = get(Ids[i], envir = pssmMap);
        for (j in 1:length(strSeq)) {
          if (nGramOrder >= 1) {
            # generate the contribution of this position to
            # all order 1 PSSM-nGram features (20 features)
            for (amin in alphabet) {
              token = paste0("C_0_", amin);
              features[i, token] = features[i, token] + pssm[j,amin]/length(strSeq);
            }
          }
          
          if (nGramOrder >= 2 && j+1 <= length(strSeq)) {
            # generate the contribution of this position to
            # all order 2 PSSM-nGram features (400 features)
            for (amin1 in alphabet) {
              for (amin2 in alphabet) {
                token = paste0("C_0_", amin1, amin2);
                features[i, token] = features[i, token] + pssm[j,amin1] * pssm[j+1,amin2]/(length(strSeq) - 1);
              }
            }
          }

          if (nGramOrder >= 2 && j+2 <= length(strSeq)) {
            # generate the contribution of this position to
            # all order 3 PSSM-nGram features (8000 features)
            for (amin1 in alphabet) {
              for (amin2 in alphabet) {
                for (amin3 in alphabet) {
                  token = paste0("C_0_", amin1, amin2, amin3);
                  features[i, token] = features[i, token] + pssm[j,amin1] * pssm[j+1,amin2] * pssm[j+2,amin3]/(length(strSeq) - 2);
                }
              }
            }
          }
        }
      }
    }

    # if (nGDipOrder > 0) {
    # 
    #   for (i in 1:nrow(features)) {
    #     strSeq = strsplit(toString(sequences[i]), "")[[1]];
    #     for (j in 1:length(strSeq)) {
    #       if (!(exists(strSeq[j], envir = alphaMap))) {
    #         next;
    #       }
    # 
    #       for (k in 1:nGDipOrder) {
    #         if (j+1+k > length(strSeq)) {
    #           break;
    #         }
    #         if (!(exists(strSeq[j+1+k], envir = alphaMap))) {
    #           next;
    #         }
    #         token = paste(strSeq[j], strSeq[j+1+k], sep = "");
    #         token = paste("G", k, token, sep = "_");
    #         if (!(token %in% colnames(features))) {
    #           # create the column on demand
    #           features[token] = integer(nrow(features));
    #           nGDipCount = nGDipCount + 1;
    #         }
    #         
    #         features[i,token] = features[i,token] + 1/(length(strSeq) - k -1);
    #       }
    #     }
    #   }
    # }
    # cat(as.character(Sys.time()),">> n-Gapped-Dipeptide (nGDip) based features:", nGDipCount, "\n");
    # 
    cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");
    
    features$protection = as.factor(labels);
    names(features) = make.names(names(features));
    features$Serial = NULL
    
    return(features)
  }
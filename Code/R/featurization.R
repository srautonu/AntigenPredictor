#' Illustration of Featurization
#'
#' This function takes dataset and a list of features as input and produce a features-wise dataset. The number of columns in returned dataset is equal to the number of features in featurelist.
#'
#' @param sequences provided as dataframe
#' @param alphabet a list of aminoacids or nucleotides
#' @param seq sequence based features. by default it is true.
#' @param seqorder highest number of sequence which will be considered together
#' @param pos position specific features. by default it is true.
#' @param posorder highest number of sequence which will be considered together
#' @return a featurized dataframe
#' @export
#' @examples
#' input = list("ABCDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
#' "ACDEFGHIKLMNOPQRSTUVWY"
#' string = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "Y")
#' featuredata = featurization(input, string, seq = TRUE, pos = FALSE)
#' featuredata
featurization <-
  function(sequences, labels, alphabet, seqorder, gap, posorder) {
    features = data.frame(1:length(sequences))
    # a dummy column got created. Let us name it. We will
    # delete this column at the end
    colnames(features)[length(features)] = "Serial"
    
    alphaMap = new.env();
    for (key in alphabet) {
      assign(key, TRUE, alphaMap);
    }
    
    nSeq = 0;
    nPos = 0;
    nGap = 0;
    
    if (seqorder > 0 || posorder > 0) {
      for (i in 1:nrow(features)) {
        strSeq = strsplit(toString(sequences[i]), "")[[1]];
        for (j in 1:length(strSeq)) {
          token = "";
          for (k in 1:max(seqorder, posorder)) {
            if (j+k-1 > length(strSeq)) {
              break;
            }
            if (!(exists(strSeq[j+k-1], envir = alphaMap))) {
              break;
            }

            token = paste(token, strSeq[j+k-1],sep = "");
            
            # update the seqorder feature count
            if (nchar(token) <= seqorder) {
              countToken = paste("C", 0, token, sep = "_")
              if (!(countToken %in% colnames(features))) {
                # create the column on demand
                features[countToken] = integer(nrow(features));
                nSeq = nSeq + 1;
              }
              features[i,countToken] = features[i,countToken] + 1;
            }
            
            # update the posorder feature count
            if (j <= 10 && nchar(token) <= posorder) {
              posToken = paste("P", j, token, sep = "_");
              if (!(posToken %in% colnames(features))) {
                # create the column on demand
                features[posToken] = integer(nrow(features));
                nPos = nPos + 1;
              }
              features[i,posToken] = features[i,posToken] + 1;
            }
          }
        }
      }
    }
    cat(as.character(Sys.time()),">> n-mer based features:", nSeq, "\n");
    cat(as.character(Sys.time()),">> position specific n-mer based features:", nPos, "\n");
    
    if (gap > 0) {
  
      for (i in 1:nrow(features)) {
        strSeq = strsplit(toString(sequences[i]), "")[[1]];
        for (j in 1:length(strSeq)) {
          if (!(exists(strSeq[j], envir = alphaMap))) {
            next;
          }

          for (k in 1:gap) {
            if (j+1+k > length(strSeq)) {
              break;
            }
            if (!(exists(strSeq[j+1+k], envir = alphaMap))) {
              next;
            }
            token = paste(strSeq[j], strSeq[j+1+k], sep = "");
            token = paste("G", k, token, sep = "_");
            if (!(token %in% colnames(features))) {
              # create the column on demand
              features[token] = integer(nrow(features));
              nGap = nGap + 1;
            }
            
            features[i,token] = features[i,token] + 1;
          }
        }
      }
    }
    cat(as.character(Sys.time()),">> Gapped DPC based features:", nGap, "\n");
    
    cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");

    # features = 1 / (1 + exp(-features));
    # cat(as.character(Sys.time()),">> Converted to sigmoid\n");
    
    features$protection = as.factor(labels);
    names(features) = make.names(names(features));
    features$Serial = NULL
    
    return(features)
  }
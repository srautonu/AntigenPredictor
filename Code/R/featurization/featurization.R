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
  function(sequences, alphabet, seqorder) {
    features = data.frame(1:length(sequences))
    # a dummy column got created. Let us name it. We will
    # delete this column at the end
    colnames(features)[length(features)] = "Serial"
    
    #prepare the columns for sequence based features
    # for (s in 1:seqorder) {
    #   permu = gtools::permutations(
    #     n = length(alphabet), r = s, v = alphabet, repeats.allowed = TRUE
    #   )
    # 
    #   for (i in 1:length((permu[,1]))) {
    #     featureName = paste0(permu[i,], collapse = '');
    #     features[featureName] = integer(nrow(features));
    #   }
    # }
    # 
    # for (i in 1:nrow(features)) {
    #   strSeq = strsplit(toString(sequences[i]), "")[[1]];
    #   for (j in 1:length(strSeq)) {
    #     token = "";
    #     for (k in 1:seqorder) {
    #       if (j+k-1 > length(strSeq)) {
    #         break;
    #       }
    #       token = paste(token, strSeq[j+k-1],sep = "");
    # 
    #       if (token %in% colnames(features)) {
    #         # In rare case, a sequence may contain amino acid symbol ('X')
    #         # that is not in our alphabet. In that case, we ignore it
    # 
    #         features[i,token] = features[i,token] + 1;
    #       }
    #     }
    #   }
    # }
    
    gap = 9;
    
    # gapped dipeptide features
    # prepare the columns for gapped dipeptide based features
    permu = gtools::permutations(
      n = length(alphabet), r = 2, v = alphabet, repeats.allowed = TRUE
    )
    
    for (i in 1:length((permu[,1]))) {
      for (j in 0:gap) {
        featureName = paste0(permu[i,], collapse = '');
        featureName = paste0(featureName, "_", j);
        features[featureName] = integer(nrow(features));
      }
    }

    
    for (i in 1:nrow(features)) {
      strSeq = strsplit(toString(sequences[i]), "")[[1]];
      for (j in 1:length(strSeq)) {
        for (k in 1:gap) {
          if (j+1+k > length(strSeq)) {
            break;
          }
          token = paste(strSeq[j], strSeq[j+1+k], "_", k, sep = "");
          if (token %in% colnames(features)) {
            # In rare case, a sequence may contain amino acid symbol ('X')
            # that is not in our alphabet. In that case, we ignore it
            features[i,token] = features[i,token] + 1;
          }
        }
      }
    }
    
    features$Serial = NULL
    
    
    return(features)
  }
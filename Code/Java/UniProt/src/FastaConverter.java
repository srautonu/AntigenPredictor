import java.io.*;
import java.util.*;

/**
 * Created by mrahman on 22-Apr-17.
 */
public class FastaConverter {
    public static void printProteinsFasta(String strFastaFile, Collection<String> lstProteins) {
        Map<String, Boolean> map_Protein = new Hashtable<String, Boolean>();
        for (String protein:lstProteins) {
            map_Protein.put(protein, true);
        }

        try (BufferedReader readerFasta = new BufferedReader(new FileReader(strFastaFile))) {
            String strInputLine;
            String strId = "";
            String strCurProtein = "";
            boolean fDiscard = true;

            do {
                strInputLine = readerFasta.readLine();
                if (null == strInputLine || strInputLine.startsWith(">")) {
                    if (!fDiscard) {
                        System.out.print(strCurProtein);
                    }

                    if (null != strInputLine) {
                        fDiscard = false;
                        strCurProtein = strInputLine + "\r\n";

                        strId = strInputLine.split("\\|")[1];
                        if (!map_Protein.containsKey(strId)) {
                            fDiscard = true;
                        }
                    }
                } else {
                    strCurProtein += strInputLine + "\r\n";
                }
            } while (null != strInputLine);
        } catch (IOException e) {
            Logger.Log(e);
        }
    }


    public static Map<String, String> getProteinSequences(String strFastaFile) {
        Map<String, String> map_Protein = new Hashtable<String, String>();
        try (BufferedReader readerFasta = new BufferedReader(new FileReader(strFastaFile))) {
            String strInputLine;
            String strId = "";
            String strSequence = "";

            do {
                strInputLine = readerFasta.readLine();
                if (null == strInputLine || strInputLine.startsWith(">")) {
                    // Beginning of a new sequence encountered. Therefore,
                    // Complete the earlier sequence and report it.
                    if (!strId.isEmpty()) {
                        map_Protein.put(strId, strSequence);
                    }

                    if (null != strInputLine) {
                        // reset id/sequence for the upcoming one
                        strSequence = "";
                        strId = strInputLine.split("\\|")[1];
                    }
                } else {
                    strSequence += strInputLine;
                }
            } while (null != strInputLine);
        } catch (IOException e) {
            Logger.Log(e);
        }
        return map_Protein;
    }

    public static Map<String, String> getProteinFASTAs(String strFastaFile) {
        Map<String, String> map_Protein = new Hashtable<String, String>();
        try (BufferedReader readerFasta = new BufferedReader(new FileReader(strFastaFile))) {
            String strInputLine;
            String strId = "";
            String strFASTA = "";

            do {
                strInputLine = readerFasta.readLine();
                if (null == strInputLine || strInputLine.startsWith(">")) {
                    // Beginning of a new sequence encountered. Therefore,
                    // Complete the earlier sequence and report it.
                    if (!strId.isEmpty()) {
                        map_Protein.put(strId, strFASTA);
                    }

                    if (null != strInputLine) {
                        // reset id/sequence for the upcoming one
                        strFASTA = strInputLine + "\n";
                        strId = strInputLine.split("\\|")[1];
                        //strId = strInputLine.substring(1).split(" ")[0];
                    }
                } else {
                    strFASTA += strInputLine + "\n";
                }
            } while (null != strInputLine);
        } catch (IOException e) {
            Logger.Log(e);
        }
        return map_Protein;
    }

    public static void main(String[] args) {
        Map<String, String> map_Protein;
        Collection<String> lst_Protein = new ArrayList<String>();

        String strFastaFile;
        String strConvType;
        String strSubsetFile;

        if (args.length < 2) {
            //
            // Convertion type is CSV or FASTA or per ID FASTAs
            // <Fasta_file> should contain fasta for one or more proteins.
            // <SubsetIDs_File> optional file specifying the proteins of interest. If not specified
            // then all proteins are output
            //
            System.out.println("Usage: java FastaConverter CSV/FASTA/FASTA_PER_ID <Fasta_File> [<SubsetIDs_File>]");
            return;
        }

        strConvType = args[0];
        strFastaFile = args[1];
        strSubsetFile = (args.length >= 3 ? args[2] : "");

        if (!strSubsetFile.isEmpty()) {
            try (BufferedReader readerIds = new BufferedReader(new FileReader(strSubsetFile))) {
                String strInputLine;
                while (null != (strInputLine = readerIds.readLine())) {
                    lst_Protein.add(strInputLine);
                }
            } catch (IOException e) {
                Logger.Log(e);
            }
        }

        if (strConvType.equalsIgnoreCase("csv")) {
            map_Protein = getProteinSequences(strFastaFile);
            if (lst_Protein.isEmpty()) {
                lst_Protein = map_Protein.keySet();
            }

            for (String strProtein : lst_Protein) {
                System.out.println(strProtein + "," + map_Protein.get(strProtein));
            }
        }
        else if (strConvType.equalsIgnoreCase("fasta_per_id")) {
            map_Protein = getProteinFASTAs(strFastaFile);
            if (lst_Protein.isEmpty()) {
                lst_Protein = map_Protein.keySet();
            }

            for (String strProtein : lst_Protein) {
                String strFASTA = null;
                try {
                    strFASTA = map_Protein.get(strProtein);
                } catch (NullPointerException e) {

                }

                if (null == strFASTA || strFASTA.isEmpty())
                    continue;

                try (BufferedWriter writerFASTA = new BufferedWriter(new FileWriter(strProtein + ".fasta"))) {
                    writerFASTA.write(strFASTA);
                } catch (IOException e) {
                    Logger.Log(e);
                }
            }

        }
        else if (strConvType.equalsIgnoreCase("fasta")) {
            printProteinsFasta(strFastaFile, lst_Protein);
        }
    }
}

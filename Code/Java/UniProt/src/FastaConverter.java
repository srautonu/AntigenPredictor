import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

/**
 * Created by mrahman on 22-Apr-17.
 */
public class FastaConverter {
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

    public static void main(String[] args) {
        Map<String, String> map_Protein;
        Collection<String> lst_Protein;

        if (args.length < 1) {
            //
            // <Fasta_file> should contain fasta for one or more proteins.
            // <SubsetIDs_File> optional file specifying the proteins of interest. If not specified
            // then all proteins are output
            //
            System.out.println("Usage: java FastaConverter <Fasta_File> [<SubsetIDs_File>]");
            return;
        }

        map_Protein = getProteinSequences(args[0]);
        if (args.length >= 2) {
            lst_Protein = new ArrayList<String>();
            try (BufferedReader readerIds = new BufferedReader(new FileReader(args[1]))) {
                String strInputLine;
                while (null != (strInputLine = readerIds.readLine())) {
                    lst_Protein.add(strInputLine);
                }
            } catch (IOException e) {
                Logger.Log(e);
            }
        } else {
            lst_Protein = map_Protein.keySet();
        }

        for (String strProtein : lst_Protein) {
            System.out.println(strProtein + "," + map_Protein.get(strProtein));
        }
    }
}

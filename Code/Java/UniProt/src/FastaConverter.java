import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

/**
 * Created by mrahman on 22-Apr-17.
 */
public class FastaConverter {
    public static void main(String[] args) {
        Map<String, String> map_Protein = new Hashtable<String, String>();
        List<String> lst_Protein = new ArrayList<String>();

        if (args.length < 1) {
            //
            // <Fasta_file> should contain fasta for one or more proteins.
            //
            System.out.println("Usage: java FastaConverter <SubsetIDs_File> <Fasta_File>");
            return;
        }

        try (
                BufferedReader readerIds = new BufferedReader(new FileReader(args[0]));
                BufferedReader readerFasta = new BufferedReader(new FileReader(args[1]));
        ) {
            String strInputLine;
            String strId = "";
            String strSequence = "";

            while (null != (strInputLine = readerIds.readLine())) {
                map_Protein.put(strInputLine, "");
                lst_Protein.add(strInputLine);
            }

            //System.out.println("Uniprot,Sequence");
            do
            {
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

        for (String strProtein : lst_Protein) {
            System.out.println(strProtein + "," + map_Protein.get(strProtein));
        }
    }
}

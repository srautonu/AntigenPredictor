import java.io.*;
import java.util.*;

/**
 * Created by mrahman on 22-Apr-17.
 */
public class FastaGenerator {
    public static Map<String, String> getProteinFASTAs(String strCSVFile) {
        Map<String, String> map_Protein = new LinkedHashMap<String, String>();
        try (BufferedReader readerCSV = new BufferedReader(new FileReader(strCSVFile))) {
            String strInputLine;
            String strId;
            String strSequence;
            String strFASTA;

            // Ignore the header
            readerCSV.readLine();

            while  (null != (strInputLine = readerCSV.readLine())){
                String[] strSplits = strInputLine.split(",");

                strId = strSplits[0];
                strSequence = strSplits[1];
                strFASTA = ">" + strId + "\r\n" + strSequence;

                map_Protein.put(strId, strFASTA);
            }
        } catch (IOException e) {
            Logger.Log(e);
        }
        return map_Protein;
    }

    private static void writePerIDFASTA(
        Collection<String> lst_Protein,
        Map<String, String> map_Protein
    )
    {
        for (String strProtein : lst_Protein) {
            String strFASTA = null;
            try {
                strFASTA = map_Protein.get(strProtein);
            } catch (NullPointerException e) {

            }

            if (null == strFASTA || strFASTA.isEmpty())
                continue;

            try (BufferedWriter writerFASTA = new BufferedWriter(new FileWriter(strProtein + ".fasta"))) {
                writerFASTA.write(strFASTA + "\r\n");
            } catch (IOException e) {
                Logger.Log(e);
            }
        }
    }

    private static void writeCombinedFASTA(
            Collection<String> lst_Protein,
            Map<String, String> map_Protein

    )
    {
        try (BufferedWriter writerFASTA = new BufferedWriter(new FileWriter("combined.fasta"))) {
            for (String strProtein : lst_Protein) {
                String strFASTA = null;
                try {
                    strFASTA = map_Protein.get(strProtein);
                } catch (NullPointerException e) {

                }

                if (null == strFASTA || strFASTA.isEmpty())
                    continue;

                writerFASTA.write(strFASTA + "\r\n");
            }
        } catch (IOException e) {
            Logger.Log(e);
        }
    }

    public static void main(String[] args) {
        Map<String, String> map_Protein;
        Collection<String> lst_Protein = new ArrayList<String>();

        String strCSVFile;
        String strSubsetFile;
        boolean fPerIdFASTA = false;

        if (args.length < 1) {
            //
            // Convertion from CSV file into FASTA format
            // <CSV_File> The first 2 columns must contain the ID and sequence
            // COMBINED/PER_ID - Generate combined or per id fasta file(s). Default is combined.
            // <SubsetIDs_File> optional file specifying the proteins of interest. If not specified
            // then all proteins are output
            //
            System.out.println("Usage: java FastaGenerator <CSV_File> [COMBINED/PER_ID] [<SubsetIDs_File>]");
            return;
        }

        strCSVFile = args[0];
        fPerIdFASTA = (args.length >= 2 &&  0 == args[1].compareToIgnoreCase("PER_ID"));
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

        map_Protein = getProteinFASTAs(strCSVFile);
        if (lst_Protein.isEmpty()) {
            lst_Protein = map_Protein.keySet();
        }

        if (fPerIdFASTA) {
            writePerIDFASTA(lst_Protein, map_Protein);
        } else {
            writeCombinedFASTA(lst_Protein, map_Protein);
        }
    }
}

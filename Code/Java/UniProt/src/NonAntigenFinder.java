import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;

/**
 * Created by mrahman on 30-Mar-17.
 */
public class NonAntigenFinder {
    public static void main(String[] args)
    {
        Map<String, Boolean> map_nonAntigenSeq = new Hashtable<String, Boolean>();

        if (args.length < 2) {
            //
            // <All_Proteins_File> should be comma separated id, seq pairs. The first line is ignored (considered heading)
            //
            System.out.println("Usage: java NonAntigenFinder <NonAntigens_Fasta> <Antigens_NonAntigens_Subset_Blast_File>");
            return;
        }

        try (
            BufferedReader readerFasta = new BufferedReader(new FileReader(args[0]));
            BufferedReader readerBlastTable = new BufferedReader(new FileReader(args[1]));
        )
        {
            String strLine;

            while (null != (strLine = readerFasta.readLine()))
            {
                if (strLine.startsWith(">")) {
                    map_nonAntigenSeq.put(strLine.split("\\|")[1], true);
                }
            }

            while (null != (strLine = readerBlastTable.readLine()))
            {
//                tr|A0A088SAC4|A0A088SAC4_9ALPH	O55887	24.096	83	45	2	64	128	140	222	0.076	26.6
                String strId = strLine.split("\t")[0];
                strId = strId.split("\\|")[1];
                if (map_nonAntigenSeq.get(strId) != null)
                    map_nonAntigenSeq.put(strId, false);
            }

            for (Map.Entry<String, Boolean> entry : map_nonAntigenSeq.entrySet())
            {
                if (entry.getValue())
                    System.out.println(entry.getKey());
            }
        }
        catch (IOException e) {
            Logger.Log(e);
        }
    }
}

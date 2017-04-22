import com.sun.org.apache.xpath.internal.operations.Bool;

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
        String strAllProteinsFile;
        String strBlastTableFile;
        Map<String, String> map_nonAntigenSeq = new Hashtable<String, String>();

        if (args.length < 2) {
            //
            // <All_Proteins_File> should be comma separated id, seq pairs. The first line is ignored (considered heading)
            //
            System.out.println("Usage: java NonAntigen <All_Proteins_File> <Blast_Tabular_File>");
            return;
        }

        strAllProteinsFile = args[0];
        strBlastTableFile = args[1];

        try (
            BufferedReader readerAllProteins = new BufferedReader(new FileReader(strAllProteinsFile));
            BufferedReader readerBlastTable = new BufferedReader(new FileReader(strBlastTableFile));
        )
        {
            String strLine;

            strLine = readerAllProteins.readLine(); // Ignore the heading
            while (null != (strLine = readerAllProteins.readLine()))
            {
                String[] rgTokens = strLine.split(",");
                map_nonAntigenSeq.put(rgTokens[0], rgTokens[1]);
            }

            while (null != (strLine = readerBlastTable.readLine()))
            {
//                tr|A0A088SAC4|A0A088SAC4_9ALPH	O55887	24.096	83	45	2	64	128	140	222	0.076	26.6
                String strId = strLine.split("\t")[0];
                strId = strId.split("\\|")[1];
                if (map_nonAntigenSeq.get(strId) != null)
                    map_nonAntigenSeq.put(strId, "");
            }

            for (Map.Entry<String, String> entry : map_nonAntigenSeq.entrySet())
            {
                if (entry.getValue().length() > 0)
                    System.out.println(entry.getKey() + ",no," + entry.getValue());
            }
        }
        catch (IOException e) {
            Logger.Log(e);
        }




    }
}

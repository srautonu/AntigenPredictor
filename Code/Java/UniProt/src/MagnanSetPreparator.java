import java.io.*;
import java.util.Hashtable;
import java.util.Map;

/**
 * Created by mrahman on 02-Aug-17.
 */
public class MagnanSetPreparator {

    public static void main(String[] args)
    {
        Map<String, Integer> map_Protein = new Hashtable<String, Integer>();
        String strPathogens[] = {
           "Brucella",
           "Burkholderia",
           "Candida",
           "Malaria",
           "Tuberculosis"
        };

        try (BufferedReader readerAntigenicity = new BufferedReader(new FileReader("protection.csv"))) {
            String strInputLine;
            String strId = "";
            Integer protection = 0;

            // Ignore the header (ID,SetType,Protection)
            readerAntigenicity.readLine();

            while (true) {
                strInputLine = readerAntigenicity.readLine();
                if (null == strInputLine) {
                    break;
                }

                String[] strSplits = strInputLine.split(",");
                strId = strSplits[0];
                protection = Integer.parseInt(strSplits[2]);
                map_Protein.put(strId, protection);
            }
        } catch (IOException e) {
            Logger.Log(e);
        }

        for (String strPathogen:strPathogens) {
            BufferedWriter[] writerPathogen = {
                null,
                null
            };

            try (
                BufferedReader readerPathogen = new BufferedReader(new FileReader(strPathogen + ".csv"));
                BufferedWriter writerAntigen = new BufferedWriter(new FileWriter(strPathogen + "_Antigen.csv"));
                BufferedWriter writerNonAntigen = new BufferedWriter(new FileWriter(strPathogen + "_NonAntigen.csv"));
            ) {

                String strInputLine;
                String strId = "";
                Integer protection = 0;

                writerPathogen[0] = writerNonAntigen;
                writerPathogen[1] = writerAntigen;

                while (true) {
                    strInputLine = readerPathogen.readLine();
                    if (null == strInputLine) {
                        break;
                    }

                    strId = strInputLine.split(",")[0];
                    try {
                        writerPathogen[map_Protein.get(strId)].write(strInputLine + "\r\n");
                    } catch (NullPointerException e) {

                    }
                }
            } catch (IOException e) {
                Logger.Log(e);
            }
        }
    }
}

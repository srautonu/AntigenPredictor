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
        String strSetNames[] = {
            "PAntigens",
            "Brucella",
            "Burkholderia",
            "Candida",
            "Malaria",
            "Tuberculosis"
        };

        String strSetTags[] = {
            "PAN",
            "BRU",
            "BUR",
            "CAN",
            "MAL",
            "TUB"
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

        try (
            BufferedWriter writerAntigens = new BufferedWriter(new FileWriter("antigens.csv"));
            BufferedWriter writerNonAntigens = new BufferedWriter(new FileWriter("nonAntigens.csv"));
            )
        {
            BufferedWriter[] writer = {
                writerNonAntigens,
                writerAntigens
            };

            writer[0].write("ID,Type,Sequence\r\n");
            writer[1].write("ID,Type,Sequence\r\n");

            for (int i = 0; i < strSetNames.length; i++) {
                String strSetName = strSetNames[i];
                String strSetTag = strSetTags[i];

                try (BufferedReader readerPathogen = new BufferedReader(new FileReader(strSetNames[i] + ".csv")))
                {
                    String strInputLine;
                    String strId = "";
                    String strSequence = "";

                    while (true) {
                        strInputLine = readerPathogen.readLine();
                        if (null == strInputLine) {
                            break;
                        }

                        String[] strSplits = strInputLine.split(",");
                        strId = strSplits[0];
                        strSequence = strSplits[1];
                        try {
                            writer[map_Protein.get(strId)].write(strId + "," + strSetTag + "," + strSequence + "\r\n");
                        } catch (NullPointerException e) {

                        }
                    }
                } catch (IOException e) {
                    Logger.Log(e);
                }
            }
        } catch (IOException e) {
            Logger.Log(e);
        }
    }
}

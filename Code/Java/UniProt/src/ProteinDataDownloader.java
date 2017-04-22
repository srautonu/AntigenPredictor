
// Taxon Id:
//     2 --> Bacteria
// 10239 --> Viruses

import java.io.*;

public class ProteinDataDownloader {
    public static void main(String[] args)
    {
        if (args.length <= 0) {
            Logger.Log("Usage: java ProteinDownloader <FileWithProteinIds>");
            return;
        }

        try (
            BufferedReader protIdReader = new BufferedReader(new FileReader(args[0]));
            BufferedWriter protDataWriter = new BufferedWriter(new FileWriter("data_" + args[0]));
            )
        {
            String strProteinId;
            String strProteinData;

            while (null != (strProteinId = protIdReader.readLine())) {
                Protein protein = new Protein(strProteinId);
                try {
                    protein.loadFromUniProt();
                } catch (Exception e) {
                    Logger.Log(e);
                }

                // lineage may contain commas. Hence use "" to wrap it
                strProteinData = protein.getAccession() + ","
                               + protein.getCategory()  + ","
                               + protein.getSequence()  + ","
                               + "\"" + protein.getLineage() + "\"";
                protDataWriter.write(strProteinData + "\r\n");
                protDataWriter.flush();
            }
        }
        catch (IOException e) {
            Logger.Log(e);
        }
    }
}
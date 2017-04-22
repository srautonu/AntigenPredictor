import java.io.*;

/**
 * Created by mrahman on 21-Mar-17.
 */

public class ProteinFastaDownLoader {
    public static void main(String[] args)
    {
        if (args.length <= 0) {
            Logger.Log("Usage: java ProteinFastaDownLoader <FileWithProteinIds>");
            return;
        }

        try (BufferedReader protIdReader = new BufferedReader(new FileReader(args[0])))
        {
            String strProteinId;
            String strProteinData;

            while (null != (strProteinId = protIdReader.readLine())) {
                Protein protein = new Protein(strProteinId);
                String strFasta = protein.getFasta();
                try (BufferedWriter protDataWriter = new BufferedWriter(new FileWriter(strProteinId + ".fasta")))
                {
                    protDataWriter.write(strFasta);
                } catch (Exception e) {
                    Logger.Log(e);
                }
            }
        }
        catch (IOException e) {
            Logger.Log(e);
        }
    }
}

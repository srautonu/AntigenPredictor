import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Created by mrahman on 22-Apr-17.
 */
public class FastaConverter {
    public static void main(String[] args) {
        if (args.length < 1) {
            //
            // <Fasta_file> should contain fasta for one or more proteins.
            //
            System.out.println("Usage: java FastaConverter <Fasta_file>");
            return;
        }

        try (
                BufferedReader readerFasta = new BufferedReader(new FileReader(args[0]));
        ) {
            String strInputLine;
            String strId = "";
            String strSequence = "";

            System.out.println("Uniprot,Sequence");

            do
            {
                strInputLine = readerFasta.readLine();
                if (null == strInputLine || strInputLine.startsWith(">")) {
                    // Beginning of a new sequence encountered. Therefore,
                    // Complete the earlier sequence and report it.
                    if (!strId.isEmpty()) {
                        System.out.println(strId + "," + strSequence);
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

    }
}

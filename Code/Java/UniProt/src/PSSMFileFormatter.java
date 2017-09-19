import java.io.*;

/**
 * Created by mrahman on 19-Sep-17.
 */
public class PSSMFileFormatter {

    private static boolean convert(String strPssmInFile) {
        boolean fRet = true;
        String strPssmOutFile = strPssmInFile + ".csv";
        String strAminHeader;
        String strRow;

        try (
            BufferedReader readerPSSM = new BufferedReader(new FileReader(strPssmInFile));
            BufferedWriter writerPSSM = new BufferedWriter(new FileWriter(strPssmOutFile))
        ) {
            // Ignore the first (blank) line
            readerPSSM.readLine();

            // Ignore the second (description) line
            readerPSSM.readLine();

            strAminHeader = readerPSSM.readLine();
            strAminHeader = strAminHeader.replaceAll("\\s+", ",");
            strAminHeader = strAminHeader.substring(1, 40);
            writerPSSM.write(strAminHeader + "\r\n");

            while (null != (strRow = readerPSSM.readLine()) && !strRow.isEmpty()) {
                strRow = strRow.replaceAll("\\s+", ",");
                String[] strSplits = strRow.split(",");

                String strTemp = "";
                // Indices 3 to 22 contains the 20 amio acid related PSSM values for this row
                for (int i = 3; i < 23; i++) {
                    strTemp += strSplits[i] + (i == 22 ? "\r\n" : ",");
                }

                writerPSSM.write(strTemp);
            }
        } catch (IOException e) {
            Logger.Log(e);
            fRet = false;
        }

        return fRet;
    }

    public static void main(String[] args) {

        if (args.length < 1) {
            //
            // <PSSM_File_List> is a text file listing all the PSSM files that
            // should be converted. Full or partial path can be given
            //
            // In the same path a .csv file will get created for each PSSM file
            //
            System.out.println("Usage: java PSSMFileFormatter <PSSM_File_List>");
            return;
        }

        String strPssmFilesList = args[0];

        try (BufferedReader readPssmFilesList = new BufferedReader(new FileReader(strPssmFilesList))) {
            String strPssmFile;
            boolean fConverted = false;
            while (null != (strPssmFile = readPssmFilesList.readLine())) {
                Logger.Log("Converting " + strPssmFile);
                fConverted = convert(strPssmFile);
                if (fConverted) {
                    Logger.Log("Done.");
                }
                else {
                    Logger.Log("Could not convert file: " + strPssmFile);
                }
            }

        } catch (IOException e) {
            Logger.Log(e);
        }
    }
}

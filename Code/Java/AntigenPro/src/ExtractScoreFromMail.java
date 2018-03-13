import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ExtractScoreFromMail {
    public static void main(String[] args) {


//        Name: BH13320
//
//        Amino Acids:
//        MKINEFNEYIKEARSFDIDRMHGMRQRMRIAMALTVLFGLMTIALALAVAALTPLKTVEPFVIRVDNSTGIIETVSALKETPNDYDEAITRYFASKYVRAREGFQLSEAEHNFRLVSLLSSPEEQSRFAKWYAGNNPESPQNIYQNMIATVTIKSISFLSKDLIQVRYYKTVRELNDKENISHWVSILNFSYINAQISTQDRLINPLGFQVSEYRSDPEVIQ
//
//        Predicted Probability of Antigenicity:
//        0.850092


        try (BufferedReader mailReader = new BufferedReader(new FileReader("AntigenPRO.mbox"))) {

            String strLine = "";
            String strId = "";
            double score = 0;

            while (true) {
                strLine = mailReader.readLine();
                if (null == strLine)
                    break;
                if (strLine.startsWith("Name:")) {
                    strId = strLine.split(": ")[1];
                    for (int i = 0; i < 5; i++)
                        mailReader.readLine();
                    score = Double.parseDouble(mailReader.readLine());

                    System.out.println(strId + "," + score);
                }
            }



        } catch(IOException e) {
            System.out.println(e);
        }

    }
}

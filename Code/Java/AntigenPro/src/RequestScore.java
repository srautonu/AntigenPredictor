import java.io.*;
import java.net.*;
import java.util.*;

import java.nio.charset.StandardCharsets;

public class RequestScore {

    public static Map<String, String> getProteinSequences(String strCSVFile) {
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
                strSequence = strSplits[2];
                map_Protein.put(strId, strSequence);
            }
        } catch (IOException e) {
            Logger.Log(e);
        }
        return map_Protein;
    }

    public static void main(String[] args) throws Exception {
        Map<String, String> map_Protein = getProteinSequences("Bartonella_Antigen.csv");

        for (Map.Entry<String, String> entry : map_Protein.entrySet()) {

            Logger.Log("Preparing request for " + entry.getKey() + " ...");

            URL url = new URL("http://scratch.proteomics.ics.uci.edu/cgi-bin/new_server/sql_predict.cgi");
            URLConnection con = url.openConnection();
            HttpURLConnection http = (HttpURLConnection) con;
            http.setRequestMethod("POST"); // PUT is another valid option
            http.setDoOutput(true);

            StringJoiner sj = new StringJoiner("&");

            sj.add(URLEncoder.encode("email", "UTF-8") + "="
                    + URLEncoder.encode("saifur80@gmail.com", "UTF-8"));

            sj.add(URLEncoder.encode("ant", "UTF-8") + "="
                    + URLEncoder.encode("on", "UTF-8"));

            sj.add(URLEncoder.encode("query_name", "UTF-8") + "="
                    + URLEncoder.encode(entry.getKey(), "UTF-8"));

            sj.add(URLEncoder.encode("amino_acids", "UTF-8") + "="
                    + URLEncoder.encode(entry.getValue(), "UTF-8"));

            byte[] out = sj.toString().getBytes(StandardCharsets.UTF_8);
            int length = out.length;

            http.setFixedLengthStreamingMode(length);
            http.setRequestProperty("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

            Logger.Log("Connecting ...");
            http.connect();
            try (OutputStream os = http.getOutputStream()) {
                os.write(out);
            }

            String strResponse = "";
            try (InputStream reader = http.getInputStream()) {
                strResponse = new String(reader.readAllBytes());
            }

            Logger.Log("Disconnecting ...");
            http.disconnect();
            Logger.Log("Going to sleep for 60 seconds ...");
            Thread.sleep(60000);
        }
    }
}

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.*;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;

/**
 * Created by mrahman on 04-Jan-17.
 */
public class Protein {
    private static final String UNIPROT_SERVICE = "http://www.uniprot.org/uniprot";
    private static XPathExpression LINEAGE_XPATH;
    private static XPathExpression SEQUENCE_XPATH;

    private String _strAccId;
    private String _strSequence;
    private String _strCategory;
    private String _strLineage;

    static {
        XPathFactory xpf = XPathFactory.newInstance();
        XPath xp = xpf.newXPath();
        try {
            LINEAGE_XPATH = xp.compile("uniprot/entry/organism/lineage/taxon");
            SEQUENCE_XPATH = xp.compile("uniprot/entry/sequence");
        }
        catch (XPathExpressionException e) {
            Logger.Log(e);
        }
    }

    public Protein(String strAccId) {
        _strAccId = strAccId;
        _strSequence = "";
        _strCategory = "Unknown";
        _strLineage = "";
    }

    public String getAccession() {
        return _strAccId;
    }

    public String getSequence() {
        return _strSequence;
    }

    public String getCategory() {
        return _strCategory;
    }

    public String getLineage() {
        return _strLineage;
    }

    public String getFasta() throws IOException
    {
        //"http://www.uniprot.org/uniprot/?format=fasta&query=accession:P32722";
        URL url = new URL("http://www.uniprot.org/uniprot/?format=fasta&query=accession:" + _strAccId);
        int c, size, i;

        HttpURLConnection conn = connectToUniprot(url);
        InputStream inStream = conn.getInputStream();

        char[] fasta = new char[15000];
        for (i = 0; (c = inStream.read()) != -1; i++) {
            fasta[i] = (char)c;
        }

        return new String(fasta, 0, i);
    }

    public HttpURLConnection connectToUniprot(URL url) throws IOException
    {
        Logger.Log("Connecting for " + _strAccId);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.connect();

        int status = conn.getResponseCode();
        while (true) {
            int wait = 0;
            String header = conn.getHeaderField("Retry-After");
            if (header != null)
                wait = Integer.valueOf(header);
            if (wait == 0)
                break;
            Logger.Log("Waiting (" + wait + ")...");
            conn.disconnect();
            try {
                Thread.sleep(wait * 1000);
            }
            catch (InterruptedException e) {
                Logger.Log(e);
            }
            conn = (HttpURLConnection) url.openConnection();
            conn.connect();
            status = conn.getResponseCode();
        }
        if (status == HttpURLConnection.HTTP_OK) {
            Logger.Log("Connection established.");
        }
        else {
            Logger.Log("Failed: " + conn.getResponseMessage());
        }

        return conn;
    }

    public void loadFromUniProt() throws
            IOException,
            ParserConfigurationException,
            SAXException,
            XPathExpressionException {

        //"http://www.uniprot.org/uniprot/?format=xml&query=accession:P32722";
        String[] propName  = { "format", "query" };
        String[] propValue = { "xml", "accession:" + _strAccId};

        StringBuilder locationBuilder = new StringBuilder(UNIPROT_SERVICE + "/?");
        for (int i = 0; i < propName.length; i++) {
            String strName = URLEncoder.encode(propName[i], "UTF-8");
            String strValue = URLEncoder.encode(propValue[i], "UTF-8");

            if (i > 0) {
                locationBuilder.append('&');
            }
            locationBuilder.append(strName).append('=').append(strValue);
        }

        URL url = new URL(locationBuilder.toString());
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc;

        HttpURLConnection conn = connectToUniprot(url);
        InputStream reader = conn.getInputStream();
        doc = db.parse(reader);

        NodeList nodeList = (NodeList) SEQUENCE_XPATH.evaluate(doc, XPathConstants.NODESET);
        String seq = nodeList.item(0).getTextContent();
        _strSequence = seq.replaceAll("\\s", "");
        nodeList = (NodeList) LINEAGE_XPATH.evaluate(doc, XPathConstants.NODESET);

        for (int i = 0; i < nodeList.getLength(); i++) {
            String taxon = nodeList.item(i).getTextContent();
            _strLineage += "<" + taxon + ">";
        }

        if (_strLineage.contains("Bacteria")) {
            _strCategory = "Bacteria";
        } else if (_strLineage.contains("Viruses")) {
            _strCategory = "Virus";
        }

        conn.disconnect();
    }
}

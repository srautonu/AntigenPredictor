import java.io.*;
import java.util.*;

/**
 * Created by mrahman on 18-Apr-17.
 */
//
// Input is the blast result of viral/bacteria/tumor antigen (non-antigen) blasted against the same set
// The rows of the input wll represent edges. Get the list of vertices and edges by processing each row
// Then find the maximum independent set of vertices using a Greedy approximation algorithm
//
public class ProteinGraph {
    static Map<String, Map<String, Double>> map_Protein = new Hashtable<String, Map<String, Double>>();
    static Map<String, Integer> map_ProteinDegree = new Hashtable<String, Integer>();
    static List<String> lst_maxIndependentSet = new ArrayList<String>(18000);

    private static void addProtein(String strProtein)
    {
        if (null == map_Protein.get(strProtein)) {
            map_Protein.put(strProtein, new Hashtable<String, Double>());
            map_ProteinDegree.put(strProtein, 0);
        }
    }

    private static void addProteinLink(String strProtein1, String strProtein2)
    {
        Map<String, Double> edgeMap1;
        Map<String, Double> edgeMap2;

        addProtein(strProtein1);
        addProtein(strProtein2);

        edgeMap1 = map_Protein.get(strProtein1);
        edgeMap2 = map_Protein.get(strProtein2);

        //
        // We do not consider self-loops.
        // Also, no operation needed if the edge has already been added. (i.e not a multi-graph)
        //
        if (!strProtein1.equalsIgnoreCase(strProtein2)
            && null == edgeMap1.get(strProtein2) && null == edgeMap2.get(strProtein1)) {

            edgeMap1.put(strProtein2, 1.0);
            edgeMap2.put(strProtein1, 1.0);

            map_ProteinDegree.put(strProtein1, map_ProteinDegree.get(strProtein1) + 1);
            map_ProteinDegree.put(strProtein2, map_ProteinDegree.get(strProtein2) + 1);
        }
    }

    private static void removeNeighbours(String strProtein)
    {
        Map<String, Double> edgeMapProt = map_Protein.get(strProtein);
        ArrayList<String> lstNeighbours = new ArrayList<String>(edgeMapProt.keySet());

        for (String strNeighbour:lstNeighbours)
        {
            removeProtein(strNeighbour);
        }
    }

    private static void removeProtein(String strProtein)
    {
        Map<String, Double> edgeMapProt;
        Map<String, Double> edgeMapProt2;

        edgeMapProt = map_Protein.get(strProtein);
        for (Map.Entry<String, Double> entry : edgeMapProt.entrySet())
        {
            String strProtein2 = entry.getKey();
            edgeMapProt2 = map_Protein.get(strProtein2);

            edgeMapProt2.remove(strProtein);
            map_ProteinDegree.put(strProtein2, map_ProteinDegree.get(strProtein2) - 1);
        }

        map_Protein.remove(strProtein);
        map_ProteinDegree.remove(strProtein);
    }

    private static String getMinDegreeVertex()
    {
        int minDegree = Integer.MAX_VALUE;
        String strMinDegProtein = new String("Unknown");

        for (Map.Entry<String, Integer> entry : map_ProteinDegree.entrySet())
        {
            if (entry.getValue() < minDegree) {
                strMinDegProtein = entry.getKey();
                minDegree = entry.getValue();
            }
        }

        return strMinDegProtein;
    }

    public static void main(String[] args)
    {

        if (args.length < 2) {
            //
            // <All_Proteins_File> should contain the list of protein-ids.
            //
            System.out.println("Usage: java ProteinGraph <All_Proteins_Fasta> <BlastSelf_Tabular_File>");
            return;
        }

        try (
                BufferedReader readerAllProteins = new BufferedReader(new FileReader(args[0]));
                BufferedReader readerBlastTable = new BufferedReader(new FileReader(args[1]));
        )
        {
            String strInputLine;
            String strQueryId;
            String strSubjectId;

            while (null != (strInputLine = readerAllProteins.readLine()))
            {
                if (strInputLine.startsWith(">")) {
                    addProtein(strInputLine.split("\\|")[1]);
                }
            }

            while (null != (strInputLine = readerBlastTable.readLine()))
            {
                // sp|A3R4N4|LT_POVKI	A3R4N4	100.000	641	0	0	1	641	1	641	0.0	1333
                String[] strTokens = strInputLine.split("\t");

                strQueryId = strTokens[0].split("\\|")[1];
                strSubjectId = strTokens[1];
                Double pIdent = Double.parseDouble(strTokens[2]);

                //if (pIdent > 50.0)
                    addProteinLink(strQueryId, strSubjectId);
            }

            int temp = 0;
            for (int x:map_ProteinDegree.values())
                    temp += x;
            System.out.println("Average degree = " + temp/(2 * map_Protein.size()));

            //
            // Approximation algorithm for finding the maximum independent set
            //
            while (map_Protein.size() > 0) {
                String strMinDegVertex = getMinDegreeVertex();
                lst_maxIndependentSet.add(strMinDegVertex);
                removeNeighbours(strMinDegVertex);
                removeProtein(strMinDegVertex);
            }

            for (String strProtein : lst_maxIndependentSet) {
                System.out.println(strProtein);
            }

        }
        catch (IOException e) {
            Logger.Log(e);
        }


    }
}

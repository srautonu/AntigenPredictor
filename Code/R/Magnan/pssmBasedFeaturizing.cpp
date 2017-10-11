#include <stdio.h>
#include <string.h>
#include <math.h>

#define ARRAYSIZE(x) (sizeof(x) / sizeof((x)[0]))

#define MAX_PROTEIN_COUNT 1500
#define MAX_PROTEIN_LEN 2000
#define MAX_PROTEIN_ID_LEN 15
#define MAX_PROTEIN_TYPE_LEN 5
#define MAX_FEATURE_COUNT 10000
#define MAX_FEATURE_NAME_LEN 7

#define BASE_FOLDER "C:\\GitHub\\AntigenPredictor\\Code\\R\\Magnan\\"

//
// For training data set featurization
//
//#define ANTIGEN_FILE        BASE_FOLDER "antigens.csv"
//#define NON_ANTIGEN_FILE    BASE_FOLDER "nonAntigens.csv"
//#define PSSM_FOLDER         BASE_FOLDER "TrainingSetPSSMs"
//#define FEATURIZED_FILE     BASE_FOLDER "featurized_pssm_nGrams.csv"

//
// For test data set featurization. Uncomment when needed, and comment out the above block
//
#define ANTIGEN_FILE        BASE_FOLDER "Bartonella_Antigen.csv"
#define NON_ANTIGEN_FILE    BASE_FOLDER "Bartonella_NonAntigen.csv"
#define PSSM_FOLDER         BASE_FOLDER "TestSetPSSMs"
#define FEATURIZED_FILE     BASE_FOLDER "testFeaturized_pssm_nGrams.csv"

struct Protein {
    char id[MAX_PROTEIN_ID_LEN + 1];
    char type[MAX_PROTEIN_TYPE_LEN + 1];
    char seq[MAX_PROTEIN_LEN + 1];
    int fProtection;
    double pssm[MAX_PROTEIN_LEN][20];
    double featureVector[MAX_FEATURE_COUNT];
};

Protein _prot[MAX_PROTEIN_COUNT];
int _nProteins;

char _featureName[MAX_FEATURE_COUNT][MAX_FEATURE_NAME_LEN + 1];
int _nFeatures;

const char _amins[] = { 'A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V' };

void loadPSSM(Protein *pProt);

void loadProteinsInternal(char *pStrProteinFile, int fProtection)
{
    char buf[2048];

    FILE *fpProteins = fopen(pStrProteinFile, "r");

    fgets(buf, ARRAYSIZE(buf), fpProteins);
    while (NULL != fgets(buf, ARRAYSIZE(buf), fpProteins)) {
        strcpy(_prot[_nProteins].id, strtok(buf, ","));
        strcpy(_prot[_nProteins].type, strtok(NULL, ","));
        strcpy(_prot[_nProteins].seq, strtok(NULL, "\n"));
        _prot[_nProteins].fProtection = fProtection;
        loadPSSM(&_prot[_nProteins]);
        _nProteins++;
    }

    fclose(fpProteins);
}

void loadProteins()
{
    _nProteins = 0;
    loadProteinsInternal(ANTIGEN_FILE, 1);
    loadProteinsInternal(NON_ANTIGEN_FILE, 0);
}

void loadPSSM(Protein *pProt)
{
    char buf[1024];
    FILE *fpPSSM = NULL;
    int i, j;

    sprintf(buf, "%s\\%s.fasta.pssm.csv", PSSM_FOLDER, pProt->id);
    fpPSSM = fopen(buf, "r");

    i = 0;
    fgets(buf, sizeof(buf), fpPSSM);
    while (NULL != fgets(buf, sizeof(buf), fpPSSM)) {
        char *p = strtok(buf, ",\n");
        j = 0;
        do {
            double x = atof(p);
            pProt->pssm[i][j++] = 1 / (1 + exp(-x));
            p = strtok(NULL, ",\n");
        } while (p);
        i++;
    }

    fclose(fpPSSM);
}

void storeFeaturized()
{
    FILE *fpFeaturized = fopen(FEATURIZED_FILE,"w");
    int i, j;

    //
    // Column headers
    //
    fprintf(fpFeaturized, "ID,Type");
    for (i = 0; i < _nFeatures; i++) {
        fprintf(fpFeaturized, ",%s", _featureName[i]);
    }
    fprintf(fpFeaturized, ",protection\n");


    for (i = 0; i < _nProteins; i++) {

        fprintf(fpFeaturized, "%s,%s", _prot[i].id, _prot[i].type);
        for (j = 0; j < _nFeatures; j++) {
            fprintf(fpFeaturized, ",%lf", _prot[i].featureVector[j]);
        }
        fprintf(fpFeaturized, ",%d\n", _prot[i].fProtection);
    }

    fclose(fpFeaturized);
}

void nGDipFeaturize(Protein *pProt, int nGDipOrder)
{
    int u, v, i, k, l;

    printf("Featurizing protein %s ... ", pProt->id);

    memset(pProt->featureVector, 0, sizeof(pProt->featureVector));

    int seqLen = strlen(pProt->seq);
    for (i = 0; i < seqLen; i++) {
        for (k = 1; k <= nGDipOrder && i + 1 + k < seqLen; k++) {
            l = (k - 1) * ARRAYSIZE(_amins) * ARRAYSIZE(_amins);
            for (u = 0; u < ARRAYSIZE(_amins); u++) {
                for (v = 0; v < ARRAYSIZE(_amins); v++) {
                    pProt->featureVector[l] += pProt->pssm[i][u] * pProt->pssm[i+1+k][v];
                    l++;
                }
            }
        }
    }
    
    printf("Done.\n");
}

void nGDipFeaturizeAll(int nGDipOrder)
{
    int i, k, u, v;

    //
    // nGDip feature names
    //
    _nFeatures = 0;
    for (k = 1; k <= nGDipOrder; k++) {
        for (u = 0; u < ARRAYSIZE(_amins); u++) {
            for (v = 0; v < ARRAYSIZE(_amins); v++) {
                sprintf(_featureName[_nFeatures], "G_%d_%c%c", k, _amins[u], _amins[v]);
                _nFeatures++;
            }
        }
    }

    for (i = 0; i < _nProteins; i++) {
        nGDipFeaturize(&_prot[i], nGDipOrder);
    }
}

void nGramsContribPerPosition(Protein *pProt, int curPos, int *pCurFeatureId, int nGramOrder, int curOrder, double curScore)
{
    int u;

    if (curOrder >= nGramOrder) {
        return;
    } 
    else if (curPos >= strlen(pProt->seq)) {
        //
        // Since there are no more positions, but ngram order still
        // not exhausted, the feature id for the ignored ngrams should
        // be added.
        //
        if (curPos >= strlen(pProt->seq)) {
            int k = nGramOrder - curOrder;
            int n = ARRAYSIZE(_amins);
            int t = 1;

            while (k--) {
                t *= n;
            }

            *pCurFeatureId += n * (t - 1) / (n - 1);
        }
        return;
    }

    for (u = 0; u < ARRAYSIZE(_amins); u++) {

        double tScore = curScore * pProt->pssm[curPos][u];

        pProt->featureVector[*pCurFeatureId] += tScore;
        ++(*pCurFeatureId);

        nGramsContribPerPosition(pProt, curPos + 1, pCurFeatureId, nGramOrder, curOrder + 1, tScore);
    }
}

void nGramsFeaturize(Protein *pProt, int nGramOrder)
{
    int i, len, iFeature;

    printf("Featurizing protein %s ... ", pProt->id);
    memset(pProt->featureVector, 0, sizeof(pProt->featureVector));

    len = strlen(pProt->seq);

    for (i = 0; i < len; i++) {
        iFeature = 0;
        nGramsContribPerPosition(pProt, i, &iFeature, nGramOrder, 0, 1);
    }

    printf("Done.\n");

}

void nGramsFeatureColumns(int nGramOrder, int curOrder, char* curName)
{
    int u;

    if (curOrder >= nGramOrder) {
        return;
    }

    for (u = 0; u < ARRAYSIZE(_amins); u++) {
        curName[curOrder] = _amins[u];

        sprintf(_featureName[_nFeatures], "C_0_%s", curName);
        _nFeatures++;

        nGramsFeatureColumns(nGramOrder, curOrder + 1, curName);

        curName[curOrder] = '\0';
    }
}


void nGramsFeaturizeAll(int nGramOrder)
{
    char buf[MAX_FEATURE_NAME_LEN + 1] = {0};
    int i;

    nGramsFeatureColumns(nGramOrder, 0, buf);

    for (i = 0; i < _nProteins; i++) {
        nGramsFeaturize(&_prot[i], 3);
    }
}

int main()
{
    printf("Loading proteins ... ");
    loadProteins();
    printf("Done.\n");

    //printf("Featurizing proteins ... \n");
    //nGDipFeaturizeAll(25);
    //printf("Featurizing completed.\n");

    printf("Featurizing proteins ... \n");
    nGramsFeaturizeAll(3);
    printf("Featurizing completed.\n");


    printf("Saving features to %s ... ", FEATURIZED_FILE);
    storeFeaturized();
    printf("Done.\n");


    return 0;
}
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define ARRAYSIZE(x) (sizeof(x) / sizeof((x)[0]))

#define MAX_PROTEIN_COUNT 1500
#define MAX_PROTEIN_LEN 2000
#define MAX_PROTEIN_ID_LEN 15
#define MAX_PROTEIN_TYPE_LEN 5
#define MAX_FEATURE_COUNT 10000
#define MAX_FEATURE_NAME_LEN 7

#define BASE_FOLDER "C:\\GitHub\\AntigenPredictor\\Code\\R\\Magnan\\"
#define ANTIGEN_FILE        BASE_FOLDER "antigens.csv"
#define NON_ANTIGEN_FILE    BASE_FOLDER "nonAntigens.csv"
#define PSSM_FOLDER         BASE_FOLDER "TrainingSetPSSMs"
#define FEATURIZED_FILE     BASE_FOLDER "featurized_pssm_nGDip25.csv"

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

int main()
{
    printf("Loading proteins ... ");
    loadProteins();
    printf("Done.\n");

    printf("Featurizing proteins ... \n");
    nGDipFeaturizeAll(25);
    printf("Featurizing completed.\n");

    printf("Saving features to %s ... ", FEATURIZED_FILE);
    storeFeaturized();
    printf("Done.\n");


    return 0;
}
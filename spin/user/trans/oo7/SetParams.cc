#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fstream.h>

#include "GenParams.h"

int NumAtomicPerComp;
int NumConnPerAtomic;
int DocumentSize;
int ManualSize;

int NumAssmPerAssm;
int NumCompPerAssm;

int NumCompPerModule;
int NumAssmLevels;
int TotalModules;

int TotalCompParts;
int TotalAtomicParts;

int TotalConnections;
int BaseAssmPerModule;
int TotalBaseAssm;
int EstAtomToCard;
int EstAtomFromCard;
int EstUsedInCard;

int MultiSleepTime;

int CachePages = 5000;

//////////////////////////////////////////////////////////////////
// Set generation parameters for the benchmark.
//
// Assumes a configuration file "OO7.config" of the
// following format:
//
// NumAssmPerAssm     	n1
// NumCompPerAssm     	n2
// NumCompPerModule   	n3
// NumAssmLevels      	n4
// TotalModules       	n5
// NumAtomicPerComp	n6
// NumConnPerAtomic 	n7
// DocumentSize		n8
// ManualSize		n9
// MultiSleepTime	n10  	(currently commented out!)
//
// where n1 through n10 are integers.  The order of
// parameters is critical!  (This is a dumb parser
// after all.) 
//////////////////////////////////////////////////////////////////

void SetParams(char* configFileName) {

        FILE *configFile;

        configFile = fopen(configFileName, "r");
        if (!configFile) {
          fprintf(stderr, "Couldn't open config file: %s\n",configFileName);
          exit(1);
        }

        // Get parameters.
        fscanf(configFile, "%*s %d\n", &NumAssmPerAssm);
        printf("NumAssmPerAssm = %d.\n", NumAssmPerAssm); 

        fscanf(configFile, "%*s %d\n", &NumCompPerAssm); 
        printf("NumCompPerAssm = %d.\n", NumCompPerAssm); 

        fscanf(configFile, "%*s %d\n", &NumCompPerModule); 
        printf("NumCompPerModule = %d.\n", NumCompPerModule); 

        fscanf(configFile, "%*s %d\n", &NumAssmLevels); 
        printf("NumAssmLevels = %d.\n", NumAssmLevels); 

        fscanf(configFile, "%*s %d\n", &TotalModules); 
        printf("TotalModules = %d.\n", TotalModules); 

        fscanf(configFile, "%*s %d\n", &NumAtomicPerComp); 
        printf("NumAtomicPerComp = %d.\n", NumAtomicPerComp); 

        fscanf(configFile, "%*s %d\n", &NumConnPerAtomic); 
        printf("NumConnPerAtomic = %d.\n", NumConnPerAtomic); 

        fscanf(configFile, "%*s %d\n", &DocumentSize); 
        printf("DocumentSize = %d.\n", DocumentSize); 

        fscanf(configFile, "%*s %d\n", &ManualSize); 
        printf("ManualSize = %d.\n", ManualSize); 

//      fscanf(configFile, "%*s %d\n", &MultiSleepTime); 
//      printf("MultiSleepTime = %d.\n", MultiSleepTime); 

        TotalCompParts = NumCompPerModule * TotalModules;
        printf("Setting TotalCompParts to %d.\n", TotalCompParts);

        TotalAtomicParts = TotalCompParts * NumAtomicPerComp;
        printf("Setting TotalAtomicParts to %d.\n", TotalAtomicParts);

// ODI's computations follow (used in estimating sizes of things)

        TotalConnections = TotalAtomicParts * NumConnPerAtomic;
        BaseAssmPerModule = 1;
        for (int i = 0; i < NumAssmLevels - 1; i++)
          BaseAssmPerModule *= NumAssmPerAssm;
        TotalBaseAssm = BaseAssmPerModule * TotalModules;
        EstAtomToCard = NumConnPerAtomic;
        EstAtomFromCard = NumConnPerAtomic + ((NumConnPerAtomic + 1) / 2);
        EstUsedInCard = (TotalBaseAssm * NumCompPerAssm * 2) / TotalCompParts;

}


/*
   old C++ version of SetParams.c

        // open file for reading
	ifstream configFile(configFileName);
	if(!configFile)
	  {
	    cout << "unable to open config file\n" ;
	    exit(1);
	  }

	// Get parameters.
	configFile.getline(buf,sizeof(buf));
	NumAssmPerAssm = (int) atoi(buf);
	cout << "NumAssmPerAssm = " << NumAssmPerAssm << "\n";

	configFile.getline(buf,sizeof(buf));
	NumCompPerAssm = (int) atoi(buf);
	cout << "NumCompPerAssm = " << NumCompPerAssm << "\n";

	configFile.getline(buf,sizeof(buf));
	NumCompPerModule = (int) atoi(buf);
	cout << "NumCompPerModule = " << NumCompPerModule << "\n";

	configFile.getline(buf,sizeof(buf));
	NumAssmLevels = (int) atoi(buf);
	cout << "NumAssmLevels = " << NumAssmLevels << "\n";

	configFile.getline(buf,sizeof(buf));
	TotalModules = (int) atoi(buf);
	cout << "TotalModules = " << TotalModules << "\n";

	configFile.getline(buf,sizeof(buf));
	NumAtomicPerComp = (int) atoi(buf);
	cout << "NumAtomicPerComp = " << NumAtomicPerComp << "\n";

	configFile.getline(buf,sizeof(buf));
	NumConnPerAtomic = (int) atoi(buf);
	cout << "NumConnPerAtomic = " << NumConnPerAtomic << "\n";

	configFile.getline(buf,sizeof(buf));
 	DocumentSize= (int) atoi(buf);
	cout << "DocumentSize = " << DocumentSize << "\n";

	configFile.getline(buf,sizeof(buf));
 	ManualSize= (int) atoi(buf);
	cout << "ManualSize = " << ManualSize << "\n";


//	configFile.get( "%*s %d\n", MultiSleepTime); 
//	cout << "MultiSleepTime = %d.\n" << MultiSleepTime;

	TotalCompParts = NumCompPerModule * TotalModules;
	cout << "Setting TotalCompParts to " << TotalCompParts << "\n";

	TotalAtomicParts = TotalCompParts * NumAtomicPerComp;
	cout << "Setting TotalAtomicParts to " << TotalAtomicParts << "\n";

// ODI's computations follow (used in estimating sizes of things)

	TotalConnections = TotalAtomicParts * NumConnPerAtomic;
	BaseAssmPerModule = 1;
	for (int i = 0; i < NumAssmLevels - 1; i++)
	  BaseAssmPerModule *= NumAssmPerAssm;
	TotalBaseAssm = BaseAssmPerModule * TotalModules;
	EstAtomToCard = NumConnPerAtomic;
	EstAtomFromCard = NumConnPerAtomic + ((NumConnPerAtomic + 1) / 2);
	EstUsedInCard = (TotalBaseAssm * NumCompPerAssm * 2) / TotalCompParts;

*/



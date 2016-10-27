#include "t1.h"

using namespace std;

struct poz{
    short int lin, col;
};

bool isPrime (const unsigned int &);
void bktPrimeDivisors (unsigned short &rez,
                       unsigned int *v,
                       const unsigned int &lgV,
                       const unsigned int &left,
                       const unsigned int &right,
                       const int &maxPrimeDivisors,
                       unsigned long long int prod,
                       int k,
                       int multipliedValues);
inline void sortVec (vector &vec);
inline bool isInMatrix (smaze &, struct poz &);

bool isPalindrom(unsigned long long number){
    unsigned long long mirroredNumber, numberCopy;
    mirroredNumber=0; numberCopy=number;

    while (numberCopy){
        mirroredNumber=mirroredNumber*10+numberCopy%10;
        numberCopy/=10;
    }
    return (number==mirroredNumber);
}

unsigned char sumBinaryFigure(unsigned long long number){
    unsigned char sum=0;

    while (number){
        sum+=number%2;
        number/=2;
    }
    return sum;
}

bool isLeapYear(unsigned short year){
    //trebuie sa fie divizibil cu 4 dar nu si cu 100
    //sau divizibil cu 400
    if ((year%4==0 && year%100) || year%400==0)
        return true;
    return false;
}

unsigned char dayOfTheWeek(unsigned short year,
                           unsigned char month,
                           unsigned char day){
    //numarul zilei din saptamana
    int rez, h, q, y, m;

    if (month<=2){
        month+=12;
        --year;
    }

    q=(int) day;
    m=(int) month;
    y=(int) year;
    h=(q+((13*(m+1))/5)+y+y/4-y/100+y/400)%7;//zeller's congruence
    rez=((h+5)%7)+1;
    return (unsigned char)rez;
}

unsigned int fibonnaci(int index){
    unsigned int fib1, fib2, fib3;
    int i;
    for (i=fib1=fib2=fib3=1; i<index-1; ++i){
        fib3=fib1+fib2;
        fib1=fib2;
        fib2=fib3;
    }

    if (!index)
        fib3=0;
    return fib3;
}

unsigned long perfectNumbers(unsigned int number){
    //humble solution based on extraordinary research
    //number>=30, macar 2 numere perfecte (6, 28)
    unsigned int prim1, prim2, p;
    unsigned long rez;
    prim2=2;

    for (p=3; 1LL * (1<<(p-1)) * ((1<<p)-1) <= number; p+=2)
        if (isPrime((1<<p)-1)){//mersenne prime => perfect number
            prim1=prim2;
            prim2=p;
        }

    rez=(1<<(prim1-1)) * ((1<<prim1)-1);
    rez+=(1<<(prim2-1)) * ((1<<prim2)-1);

    /*
    //Varianta 2, brute-force optimizat
    unsigned long rez, nr, nrCount, sum, d;
    if (number%2==0)
        nr=number-1;
        else
        nr=number;
    for (rez=nrCount=0; nrCount<2; nr-=2){}
        sum=1;//1 sigur il divide pe nr
        for (d=2; d*d<nr; ++d)
            if (nr%d==0)
                sum+=d+nr/d;
        //daca e patrat perfect, d=nr/d, adun 1 sg. data
        if (d*d==nr)
            sum+=d;
        if (sum==nr){
            ++nrCount;
            rez+=nr;
        }
    }*/
    return rez;
}

unsigned short primeDivisors(unsigned int left, unsigned int right){
    if (left>right)
        return primeDivisors (right, left);
    if (left==right || !left)
        return 1;

    //prod de nr prime pana la 29 inclusiv depaseste unsigned int (aprox 6 mld.)
    unsigned int nrPrime[MAX_ARRAY_LENGTH], lgNrPrime=0, x, d, nrPrimeDivisors;
    unsigned long long int prod;
    unsigned int maxPrimeDivisors=0, i;
    unsigned short rez=0;

    nrPrime[lgNrPrime++]=2;//primul numar prim
    for (prod=3; lgNrPrime<MAX_ARRAY_LENGTH/2; prod+=2)//prod e o var. aleatorie
        if (isPrime(prod))
            nrPrime[lgNrPrime++]=prod;

    for (prod=1, i=0; prod<=right; ++i){
        prod*=nrPrime[i];
        ++maxPrimeDivisors;
    }
    --maxPrimeDivisors;
    prod/=nrPrime[i-1];
    if (left<=prod && prod<=right){
        bktPrimeDivisors (rez, nrPrime, lgNrPrime, left, right, maxPrimeDivisors, 1, 0, 0);
        return rez;
    }

    //if not... brute force
    maxPrimeDivisors=0;
    for (x=left; x<=right; ++x){
        nrPrimeDivisors=0;
        for (d=2; d*d<x; ++d)
            if (x%d==0){
                if (isPrime(d))
                    ++nrPrimeDivisors;
                if (isPrime(x/d))
                    ++nrPrimeDivisors;
            }
        //daca e patrat perfect, d=nr/d, adun 1 sg. data
        if (d*d==x && isPrime(d))
            ++nrPrimeDivisors;
        if (nrPrimeDivisors>maxPrimeDivisors){
            maxPrimeDivisors=nrPrimeDivisors;
            rez=1;
        }
            else
            if (nrPrimeDivisors==maxPrimeDivisors)
                ++rez;
    }
    return rez;
}

void bktPrimeDivisors (unsigned short &rez,
                       unsigned int *v,
                       const unsigned &lgV,
                       const unsigned int &left,
                       const unsigned int &right,
                       const int &maxPrimeDivisors,
                       unsigned long long int prod,
                       int k,
                       int multipliedValues){
    if (prod>right)
        return;

    if (multipliedValues==maxPrimeDivisors){
        if (left)
            rez+=(unsigned long long int)right/prod - (unsigned long long int)(left-1)/prod;
            else
            rez+=(unsigned long long int)right/prod;
        return;
    }

    unsigned int i;
    for (i=k; i<lgV; ++i){
        prod*=v[i];
        bktPrimeDivisors (rez, v, lgV, left, right, maxPrimeDivisors, prod, i+1, multipliedValues+1);
        prod/=v[i];
    }
}

matrix primeTwins(unsigned int count, unsigned int lowerBound){
    unsigned int x1, x2, x3;
    unsigned int lastPrimeX3;
    bool x1IsPrime=false, x3IsPrime=false;
    matrix rez;

    rez.lines=0;
    rez.columns=2;
    x2=lowerBound+3;
    if (x2%2==0)
        ++x2;
    if (!count){
        rez.columns=0;
        return rez;
    }

    //prima pereche o tratez separat, eventual a doua daca x3 e prim
    //ca sa am x3IsPrime calculat
    while (1){
        x1=x2-2; x3=x2+2;
        if (isPrime(x2)){
            if (isPrime(x1)){
                rez.values[rez.lines][0]=x1;
                rez.values[rez.lines][1]=x2;
                ++rez.lines;
                if (count==1)
                    break;

                x3IsPrime=isPrime (x3);
                lastPrimeX3=x3;
                if (x3IsPrime){
                    rez.values[rez.lines][0]=x2;
                    rez.values[rez.lines][1]=x3;
                    ++rez.lines;
                }
                break;
            }
        }
        x2+=4;
    }
    if (count<=2)
        return rez;

    while (1){
        x2+=4;
        x1=x2-2; x3=x2+2;//x1 devine x3-ul anterior
        x1IsPrime=x3IsPrime;//incerc sa nu calculez de 2 ori acelasi lucru

        if (isPrime(x2)){
            if (lastPrimeX3!=x1)//daca x1 nu coincide cu ultimul x3 pt. care am calculat primalitatea
                x1IsPrime=isPrime (x1);
            //altfel, deja stiu daca x1 e prim in functie de x3IsPrime
            if (x1IsPrime){
                rez.values[rez.lines][0]=x1;
                rez.values[rez.lines][1]=x2;
                ++rez.lines;
                if (rez.lines==count)
                    break;
            }

            x3IsPrime=isPrime (x3);
            if (x3IsPrime){
                rez.values[rez.lines][0]=x2;
                rez.values[rez.lines][1]=x3;
                ++rez.lines;
                lastPrimeX3=x3;
                if (rez.lines==count)
                    break;
            }
        }
    }
    return rez;
}

bool areOrderedFibonnaci (vector vec){
    unsigned int i;
    int f1=0, f2=1, f3;

    if (!vec.length)
        return true;
    if (vec.length<2)
        if (vec.values[0]!=0)
            return false;
    if (vec.length>=2)
        if (vec.values[1]!=1)
            return false;

    for (i=2; i<vec.length; ++i){
        f3=f1+f2;
        if (vec.values[i]!=f3)
            return false;
        f1=f2;
        f2=f3;
    }
    return true;
}

unsigned char checkVectorInclude(vector vecOne, vector vecTwo){
    bool isIn1butNotIn2=false, isIn2ButNotIn1=false;
    unsigned int i, j, ctOne, ctTwo;

    sortVec (vecOne);
    sortVec (vecTwo);
    if (!vecOne.length || !vecTwo.length){//multimea vida e inclusa in multimea vida
        if (!vecOne.length && !vecTwo.length)
            return 0;
        if (!vecOne.length && vecTwo.length)
            return 1;
        return 2;
    }

    for (i=j=0; i<vecOne.length && j<vecTwo.length; ++i, ++j){//O(min (n, m))
        ctOne=ctTwo=1;
        while (i+1<vecOne.length && vecOne.values[i]==vecOne.values[i+1]){
            ++i;
            ++ctOne;
        }
        while (j+1<vecTwo.length && vecTwo.values[j]==vecTwo.values[j+1]){
            ++j;
            ++ctTwo;
        }

        if (vecOne.values[i]==vecTwo.values[j]){
            if (ctOne<ctTwo)
                isIn2ButNotIn1=true;
                else
                if (ctOne>ctTwo)
                    isIn1butNotIn2=true;
        }
            else{
            //valorile sunt diferite
            isIn1butNotIn2=isIn2ButNotIn1=true;
            }
    }
    if (i!=vecOne.length)//mai am elemente in primul vector
        isIn1butNotIn2=true;
        else
        if (j!=vecTwo.length)
            isIn2ButNotIn1=true;

    if (!isIn1butNotIn2 && !isIn2ButNotIn1)
        return 0;
    if (isIn2ButNotIn1 && !isIn1butNotIn2)
        return 1;
    if (isIn1butNotIn2 && !isIn2ButNotIn1)
        return 2;
    return 3;
}

bool checkIsIn(vector vec, matrix mat){
    unsigned int i, j;

    if (vec.length==mat.columns){
        for (i=0; i<mat.lines; ++i){
            for (j=0; j<vec.length; ++j)
                if (vec.values[j]!=(int)mat.values[i][j])
                    break;
            if (j==vec.length)
                return true;
        }
    }
    if (vec.length==mat.lines){
        for (j=0; j<mat.columns; ++j){
            for (i=0; i<mat.lines; ++i)
                if (vec.values[i]!=(int)mat.values[i][j])
                    break;
            if (i==mat.lines)//good, good
                return true;
        }
    }
    return false;
}

matrix rotate(matrix mat, unsigned int rotLeft, unsigned int rotRight){
    unsigned int ct, i, j, temp;
    matrix aux;
    //rotesc doar spre dreapta
    if (rotLeft<rotRight)
        rotRight-=rotLeft;
        else{
        rotLeft-=rotRight;
        //transform rotatiile spre stanga in rotatii spre dreapta
        rotLeft=rotLeft%4;
        rotRight=4-rotLeft;
        }
    rotRight=rotRight%4;
    for (ct=0; ct<rotRight; ++ct){
        for (i=0; i<mat.lines; ++i){
            for (j=0; j<mat.columns; ++j)
                aux.values[j][mat.lines-i-1]=mat.values[i][j];
        }
        temp=mat.lines; mat.lines=mat.columns; mat.columns=temp;
        for (i=0; i<mat.lines; ++i)
            for (j=0; j<mat.columns; ++j)
                mat.values[i][j]=aux.values[i][j];
    }
    return mat;
}

bool isPartOfFibonnaci(vector vec, unsigned int startingNumber){
    //can't use areOrderedFibonnaci (vector vec)
    unsigned int fib1, fib2, fib3;
    unsigned int i;
    //startingNumber>=2
    for (fib1=fib2=fib3=1; fib2<startingNumber;){
        fib3=fib1+fib2;
        fib1=fib2;
        fib2=fib3;
    }
    if (fib2!=startingNumber)//startingNumber trebuie sa fie numar Fib.
        return false;

    sortVec (vec);
    for (i=0; i<vec.length && (int)fib2==vec.values[i]; ++i){
        fib3=fib1+fib2;
        fib1=fib2;
        fib2=fib3;
    }
    if (i!=vec.length)
        return false;
    return true;
}

unsigned long setOperations(long sets[], char operations[], unsigned int x){
    unsigned long rez, i;
    switch (operations[0]){
        case 'U': rez=sets[0]|sets[1]; break;
        case 'A': rez=sets[0]&sets[1]; break;
        case '\\': rez=sets[0]&(~sets[1]); break;
        case '/': rez=sets[1]&(~sets[0]); break;
    }
    for (i=2; i<x; ++i)
        switch (operations[i-1]){
            case 'U': rez=rez|sets[i]; break;
            case 'A': rez=rez&sets[i]; break;
            case '\\': rez=rez&(~sets[i]); break;
            case '/': rez=sets[i]&(~rez); break;
        }
    return rez;
}

unsigned long bitOperations(long numbers[], char operations[], unsigned int x){
    unsigned long rez, i;
    switch (operations[0]){
        case '<': rez=(numbers[0]<<numbers[1]); break;
        case '>': rez=(numbers[0]>>numbers[1]); break;
        case '^': rez=numbers[0]^numbers[1]; break;
        case '|': rez=numbers[0]|numbers[1]; break;
        case '&': rez=numbers[0]&numbers[1]; break;
    }
    for (i=2; i<x; ++i)
        switch (operations[i-1]){
            case '<': rez=(rez<<numbers[i]); break;
            case '>': rez=(rez>>numbers[i]); break;
            case '^': rez=rez^numbers[i]; break;
            case '|': rez=rez|numbers[i]; break;
            case '&': rez=rez&numbers[i]; break;
        }
    return rez;
}

bool palindrom(long number){
    long cNumber, i;
    bool binaryRepresentation[40];
    if (number<0)
        cNumber=-number;
        else
        cNumber=number;
    for (i=0; cNumber || i<32; ++i){
        binaryRepresentation[i]=cNumber%2;
        cNumber/=2;
    }

    if (number<0){
        for (i=0; i<32; ++i)
            binaryRepresentation[i]=1-binaryRepresentation[i];
        //adun 1 in baza 2
        for (i=0; i<32 && binaryRepresentation[i]; ++i);
        binaryRepresentation[i-1]=0;
        binaryRepresentation[i]=1;
    }
    //check if it is palindrom

    for (i=0; i<16; ++i)
        if (binaryRepresentation[i]!=binaryRepresentation[32-i-1])
            return false;
    return true;
}

bool fibonnaciSpirale(matrix mat){
    unsigned int i, j, ct, limit=50;//nu pot fi mai mult de 50 de numere
    vector melc;
    ct=0; melc.length=0;
    if (mat.lines*mat.columns>limit)
        return false;

    while (melc.length<mat.lines*mat.columns-(mat.lines==mat.columns)){
        for (j=ct; j<mat.columns-ct-1; ++j)
            melc.values[melc.length++]=mat.values[ct][j];
        for (i=ct; i<mat.lines-ct-1; ++i)
            melc.values[melc.length++]=mat.values[i][mat.columns-ct-1];
        for (j=mat.columns-ct-1; j>ct; --j)
            melc.values[melc.length++]=mat.values[mat.lines-ct-1][j];
        for (i=mat.lines-ct-1; i>ct; --i)
            melc.values[melc.length++]=mat.values[i][ct];
        ++ct;
    }
    if (mat.lines==mat.columns && mat.lines%2)
        melc.values[melc.length++]=mat.values[ct][ct];
    return areOrderedFibonnaci(melc);
}



unsigned int minRouteLength(smaze maze){
    unsigned int dist[MAX_ARRAY_LENGTH_LONG][MAX_ARRAY_LENGTH_LONG];//aprox. 4 MB
    struct poz coada[MAX_ARRAY_LENGTH_LONG*MAX_ARRAY_LENGTH_LONG+1];//aprox. 4 MB
    struct poz currentNode, adjacentNode;
    unsigned int pozCoada, countCoada, k;
    short int dl[5]={-1, 0, 1, 0};
    short int dc[5]={0, 1, 0, -1};

    coada[0].lin=(short int)maze.rowOfDeparture;
    coada[0].col=(short int)maze.columnOfDeparture;
    pozCoada=0; countCoada=1;
    dist[maze.rowOfDeparture][maze.columnOfDeparture]=1;

    while (pozCoada<countCoada && !dist[maze.rowOfExit][maze.columnOfExit]){
        currentNode=coada[pozCoada++];

        for (k=0; k<4; ++k){
            adjacentNode.lin=currentNode.lin+dl[k];
            adjacentNode.col=currentNode.col+dc[k];
            if (isInMatrix (maze, adjacentNode))
                if (!dist[adjacentNode.lin][adjacentNode.col] &&
                    !maze.maze[adjacentNode.lin][adjacentNode.col]){
                    coada[countCoada++]=adjacentNode;
                    dist[adjacentNode.lin][adjacentNode.col]=dist[currentNode.lin][currentNode.col]+1;
                }
        }
    }

    return dist[maze.rowOfExit][maze.columnOfExit]-1;
}

void transformMatrix(char mat[MAX_ARRAY_LENGTH_LONG][MAX_ARRAY_LENGTH_LONG],
                     unsigned int rows,
                     unsigned int columns){
    int i, j;//fara memorie auxiliara :> :> :>
    for (i=0; i<rows; ++i)
        for (j=0; j<columns; ++j)
            if (!mat[i][j]){
                mat[i][0]=2;
                mat[0][j]=3;
            }
    for (i=rows-1; i>=0; --i)
        for (j=columns-1; j>=0; --j)
            if (mat[i][0]==2 || mat[0][j]==3)
                mat[i][j]=0;
    return;
}

bool isPrime (const unsigned int &x){//multi-purpose
    unsigned int d=3;
    if (x%2==0 && x!=2)
        return false;
    if (x<2)//0, 1 nu sunt prime
        return false;
    for (d=3; d*d<=x; d+=2)
        if (x%d==0)
            return false;
    return true;
}

inline void sortVec (vector &vec){//sorteaza crescator
    unsigned int i, j, pozMin;
    int aux;

    if (!vec.length) return;
    for (i=0; i<vec.length-1; ++i){
        pozMin=i;
        for (j=i+1; j<vec.length; ++j)
            if (vec.values[j]<vec.values[pozMin])
                pozMin=j;
        aux=vec.values[i];
        vec.values[i]=vec.values[pozMin];
        vec.values[pozMin]=aux;
    }
    return;
}

inline bool isInMatrix (smaze &maze, struct poz &a){
    if (a.lin<0 || a.lin>=(short int)maze.noOfRows)
        return false;
    if (a.col<0 || a.col>=(short int)maze.noOfColumns)
        return false;
    return true;
}

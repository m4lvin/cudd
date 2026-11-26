#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>

#include "util.h"
#include "cudd.h"

//Function wrappers around macros
DdNode *wrappedRegular(DdNode *f){
    assert(f);
    return Cudd_Regular(f);
}

void wrappedCuddRef(DdNode *f){
	assert(f);
	Cudd_Ref(f);
}

int wrappedCuddIsComplement(DdNode *f){
    return Cudd_IsComplement(f);
}

//Garbage collection hooks
int preGCHook_sample(DdManager *dd, const char *str, void *data){
	printf("Performing %s garbage collection...", str);
	return 1;
}

int postGCHook_sample(DdManager *dd, const char *str, void *data){
	printf("%s GC done\n", str);
	return 1;
}

void wrappedCuddDumpDot(DdManager *m, DdNode *f, char *filename){
	printf("filename: %s\n", filename);
	FILE *file = fopen(filename, "w");
	assert(file);
	Cudd_DumpDot(m, 1, &f, NULL, NULL, file);
	fclose(file);
}

int **allSat(DdManager *m, DdNode *n, int *nterms, int *nvars){
    CUDD_VALUE_TYPE value;
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    int num = ceil(Cudd_CountPathsToNonZero(n));
    int i=0;

    *nterms = num;
    *nvars = size;

    int **result = malloc(sizeof(int *)*num);
    assert(result);
    Cudd_ForeachCube(m, n, gen, cube, value){
        result[i] = malloc(sizeof(int *)*size);
        assert(result[i]);
        int j;
        for(j=0; j<size; j++){
            result[i][j] = cube[j];
        }
        i++;
    }

    return result;
}

int *oneSat(DdManager *m, DdNode *n, int *nvars){
    CUDD_VALUE_TYPE value;
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    int j;

    *nvars = size;

    gen = Cudd_FirstCube (m, n, &cube, &value);
    if (Cudd_IsGenEmpty(gen)) {
        Cudd_GenFree (gen);
        return NULL;
    }
    
    int *result = malloc(sizeof(int) * size);
    assert(result);
    for(j=0; j<size; j++){
        result[j] = cube[j];
    }
    Cudd_GenFree (gen);

    return result;
}

int *onePrime(DdManager *m, DdNode *l, DdNode *u, int *nvars){
    DdGen *gen;
    int *cube;
    int size = Cudd_ReadSize(m);
    int j;

    *nvars = size;

    gen = Cudd_FirstPrime(m, l, u, &cube);
    if (Cudd_IsGenEmpty(gen)) {
        Cudd_GenFree (gen);
        return NULL;
    }
    
    int *result = malloc(sizeof(int) * size);
    assert(result);
    for(j=0; j<size; j++){
        result[j] = cube[j];
    }
    Cudd_GenFree (gen);

    return result;
}


/* ------------------------------------------------------------------------
 * Manager lifetime management for the Haskell bindings.
 *
 * Every DdNode handed to Haskell with a finalizer holds one reference that
 * is released by hsCuddDerefBdd / hsCuddDerefZdd when the ForeignPtr dies.
 * GHC gives no ordering guarantee between finalizers, so a manager must not
 * be freed while such finalizers are still pending.  We therefore count the
 * live finalizer-owned nodes per manager.  hsCuddQuit only marks the manager;
 * Cudd_Quit is called as soon as that count drops to zero.
 *
 * All functions here are imported "unsafe" and finalizers run during GC, so
 * they never run concurrently with each other (GHC's GC is stop-the-world).
 * ---------------------------------------------------------------------- */

typedef struct hsMgrEntry {
    DdManager *m;
    long live;          /* number of live finalizer-owned nodes */
    int quitRequested;
    struct hsMgrEntry *next;
} hsMgrEntry;

static hsMgrEntry *hsMgrList = NULL;

static hsMgrEntry **hsMgrFind(DdManager *m){
    hsMgrEntry **p = &hsMgrList;
    while (*p && (*p)->m != m) p = &(*p)->next;
    return p;
}

static hsMgrEntry *hsMgrGet(DdManager *m){
    hsMgrEntry **p = hsMgrFind(m);
    if (!*p) {
        hsMgrEntry *e = malloc(sizeof(hsMgrEntry));
        assert(e);
        e->m = m; e->live = 0; e->quitRequested = 0; e->next = NULL;
        *p = e;
    }
    return *p;
}

static void hsMgrMaybeQuit(DdManager *m){
    hsMgrEntry **p = hsMgrFind(m);
    hsMgrEntry *e = *p;
    if (e && e->quitRequested && e->live == 0) {
        *p = e->next;
        free(e);
        Cudd_Quit(m);
    }
}

void hsCuddIncLive(DdManager *m){
    hsMgrGet(m)->live++;
}

static void hsMgrDecLive(DdManager *m){
    hsMgrEntry *e = *hsMgrFind(m);
    assert(e && e->live > 0);
    e->live--;
    hsMgrMaybeQuit(m);
}

void hsCuddDerefBdd(DdManager *m, DdNode *n){
    Cudd_IterDerefBdd(m, n);
    hsMgrDecLive(m);
}

void hsCuddDerefZdd(DdManager *m, DdNode *n){
    Cudd_RecursiveDerefZdd(m, n);
    hsMgrDecLive(m);
}

/* Request that the manager be freed once all Haskell-owned nodes are gone. */
void hsCuddQuit(DdManager *m){
    hsMgrGet(m)->quitRequested = 1;
    hsMgrMaybeQuit(m);
}

/* Number of live finalizer-owned nodes, or -1 if the manager is unknown/freed. */
long hsCuddLiveNodes(DdManager *m){
    hsMgrEntry *e = *hsMgrFind(m);
    return e ? e->live : -1;
}

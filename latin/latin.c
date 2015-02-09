/*
 * Brute force enumeration of latin "squares" of the
 * given shape.
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>

#define N 6
#define M 11
#define D (N * M)

char shape[M][M] = {
    "x   xxxxx  ",
    "xxx   x xx ",
    "  xxx xxx  ",
    "x x xx  xx ",
    "xxx  x   xx",
    "x  x x xx x",
    "   xxxxx  x",
    " xxx   x xx",
    "xx x   xxx ",
    " x xx x  xx",
    " xx xxx   x"
};

char grids[D + 1][M][M];

void printgrid(char g[M][M])
{
    int i;
    for (i = 0; i < M; i++) {
        printf("%.*s\n", M, g[i]);
    }
}

void solve(int d)
{
    int r, c, r1, c1, i, found;
    char v;
    int cands[N];

    if (d == D) {
        printf("found solution:\n");
        printgrid(grids[d]);
        return;
    }

    /* find first empty cell */
    found = 0;
    for (r = 0; r < M; r++) {
        for (c = 0; c < M; c++) {
            if (grids[d][r][c] == 'x') {
                found = 1;
                break;
            }
        }
        if (found) {
            break;
        }
    }
    assert(found);

    for (i = 0; i < N; i++) {
        cands[i] = 1;
    }
    for (r1 = 0; r1 < r; r1++) {
        v = grids[d][r1][c];
        if (v >= '1' && v <= '9') {
            cands[v - '1'] = 0;
        }
    }
    for (c1 = 0; c1 < c; c1++) {
        v = grids[d][r][c1];
        if (v >= '1' && v <= '9') {
            cands[v - '1'] = 0;
        }
    }

    for (i = 0; i < N; i++) {
        if (cands[i]) {
            found = 1;
            memcpy(grids[d + 1], grids[d], sizeof(grids[d]));
            grids[d + 1][r][c] = (char)i + '1';
            solve(d + 1);
        }
    }
}

int main(int argc, char **argv)
{
    memcpy(grids[0], shape, sizeof(shape));
    solve(0);
}

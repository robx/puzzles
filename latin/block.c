#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <pthread.h>

#define N 3
#define T 1
#define M 1000

#define D (2*N-1)
#define E (4*D)

char latin[M][N+2][N+2];
char lg[N][N];
int nl;

void solvel(int x, int y) {
	if (y >= N) {
		int i, j;
		for (i = 0; i < N; i++) {
			for (j = 0; j < N; j++) {
				latin[nl][i+1][j+1] = lg[i][j];
			}
		}
		nl++;
		return;
	}

	int i;
	int cands[N+1];
	for (i = 1; i <= N; i++) {
		cands[i] = 1;
	}
	for (i = 0; i < x; i++) {
		cands[lg[y][i]] = 0;
	}
	for (i = 0; i < y; i++) {
		cands[lg[i][x]] = 0;
	}

	int xn, yn;
	xn = x + 1;
	yn = y;
	if (xn >= N) {
		xn = 0;
		yn++;
	}
	for (i = 1; i <= N; i++) {
		if (!cands[i]) {
			continue;
		}
		lg[y][x] = i;
		solvel(xn, yn);
	}
}

int clue(char g[2*N][2*N], int x, int y, int dx, int dy) {
	int h = 0, l;
	int i;
	int c = 0;
	for (i = 1; i <= N; i++) {
		l = g[y+i*dy][x+i*dx];
		if (l > h) {
			h = l;
			c++;
		}
	}
	return c;
}

int clueN(char g[N+2][N+2], int x, int y, int dx, int dy) {
	int h = 0, l;
	int i;
	int c = 0;
	for (i = 1; i <= N; i++) {
		l = g[y+i*dy][x+i*dx];
		if (l > h) {
			h = l;
			c++;
		}
	}
	return c;
}

void clues(char g[N+2][N+2]) {
	int x, y;
	x = 0;
	for (y = 1; y <= N; y++) {
		g[y][x] = clueN(g, x, y, 1, 0);
	}
	x = N+1;
	for (y = 1; y <= N; y++) {
		g[y][x] = clueN(g, x, y, -1, 0);
	}
	y = 0;
	for (x = 1; x <= N; x++) {
		g[y][x] = clueN(g, x, y, 0, 1);
	}
	y = N+1;
	for (x = 1; x <= N; x++) {
		g[y][x] = clueN(g, x, y, 0, -1);
	}
}

void allclues() {
	int i;
	for (i = 0; i < nl; i++) {
		clues(latin[i]);
	}
}

void printg(char g[N+2][N+2]) {
	int  x, y;
	for (y = 0; y <= N+1; y++) {
		for (x = 0; x <= N+1; x++) {
			if ((x == 0 && y == 0) || (x == N+1 && y == 0)
				|| (x == 0 && y == N+1) || (x == N+1 && y == N+1)) {
				printf(" ");
			} else {
				printf("%d", g[y][x]);
			}
		}
		printf("\n");
	}
}

void prep() {
	solvel(0, 0);
	printf("computed %d latin squares\n", nl);
	allclues();
}

char defs[T][1 << E];

long found[T];

void logdef(int t, int d) {
	switch (defs[t][d]) {
	case 0:
		defs[t][d] = 1;
		break;
	case 1:
		defs[t][d] = 2;
		break;
	}
}

typedef int grid[4]; // tl, tr, br, bl

int def1(grid g, int ix) {
	int x0, y0, dx, dy;
	int i;
	int d = 0;
	int cur = g[ix], prev = g[(ix - 1 + 4) % 4], next = g[(ix + 1) % 4];
	switch (ix) {
	case 0:
		x0 = N;
		y0 = N;
		dx = -1;
		dy = 0;
		break;
	case 1:
		x0 = 1;
		y0 = N;
		dx = 0;
		dy = -1;
		break;
	case 2:
		x0 = 1;
		y0 = 1;
		dx = 1;
		dy = 0;
		break;
	case 3:
		x0 = N;
		y0 = 1;
		dx = 0;
		dy = 1;
		break;
	}

	int x, y, opx, opy;
	int c;
	for (i = N-1; i >= 0; i--) {
		x = x0 + i*dx;
		y = y0 + i*dy;
		opx = x - N*dy;
		opy = y + N*dx;
		c = latin[prev][opy][opx];
//		printf("x,y %d,%d xop,yop %d,%d v,c %d,%d\n", x, y, opx, opy, latin[cur][y][x], c);
		d <<= 1;
		d |= c == latin[cur][y][x];
	}
	i = dx;
	dx = -dy;
	dy = i;
	for (i = 0; i <= N-1; i++) {
		x = x0 + i*dx;
		y = y0 + i*dy;
		opy = y - N*dx;
		opx = x + N*dy;
		c = latin[next][opy][opx];
//		printf("x,y %d,%d xop,yop %d,%d v,c %d,%d\n", x, y, opx, opy, latin[cur][y][x], c);
		if (i == 0) {
			if ((c == latin[cur][y][x]) ^ ((d&1) == 1)) {
				return -1;
			}
		} else {
			d <<= 1;
			d |= c == latin[cur][y][x];
		}
	}
	return d;
}

int def(grid g) {
	int d1 = def1(g, 0);
	int d2 = def1(g, 1);
	int d3 = def1(g, 2);
	int d4 = def1(g, 3);
//	printf("defs: %d %d %d %d\n", d1, d2, d3, d4);
	if (d1 < 0 || d2 < 0 || d3 < 0 || d4 < 0) {
		return -1;
	}
	return (d1 << 3*D) | (d2 << 2*D) | (d3 << D) | d4;
}

void printdef(int d) {
	if (d < 0) {
		printf("invalid\n");
		return;
	}
	int i;
	for (i = E-1; i >= 0; i--) {
		printf("%c", (d >> i & 1) ? '*' : '.');
	}
	printf("\n");
}

#define R (4*N*N)

grid gs[T];

void printgs(grid g) {
	int i;
	for (i = 0; i < 4; i++) {
		printg(latin[g[i]]);
		printf("\n");
	}
}

void solve(int t, int d) {
	if (d == 4) {
//		printf("found\n");
//		printgs(gs[t]);
		found[t]++;
		int d = def(gs[t]);
//		printdef(d);
		if (d >= 0) {
			logdef(t, d);
		}
		return;
	}

	int i = 0;
	int step = 1;
	if (d == 0) {
		i = t;
		step = T;
	}

	for (; i < nl; i += step) {
		gs[t][d] = i;
		solve(t, d+1);
	}
}

void handle(int sig) {
	long f = 0;
	int i;
	for (i = 0; i < T; i++) {
		f += found[i];
	}
	fprintf(stderr, "found %ld\n", f);
	if (sig == SIGINT) {
		exit(1);
	}
}

void *solve0(void *threadid) {
	long t;
	t = (int)threadid;
	solve(t, 0);
	pthread_exit((void*)t);
}

int main(int argc, char **argv) {
	signal(SIGINT, handle);
	signal(SIGHUP, handle);
	long t;
	pthread_t threads[T];
	pthread_attr_t attr;

	prep();

	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	for (t = 0; t < T; t++) {
		pthread_create(&threads[t], &attr, solve0, (void *)t);
	}
	pthread_attr_destroy(&attr);
	for (t = 0; t < T; t++) {
		pthread_join(threads[t], NULL);
	}
	int i;
	for (i = 0; i < 1<<E; i++) {
		int c = 0;
		for (t = 0; t < T; t++) {
			c += defs[t][i];
		}
		if (c == 1) {
			printdef(i);
		}
	}
	handle(0);
	pthread_exit(NULL);
}

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <pthread.h>

#define N 4
#define T N

#define D (2*N-1)
#define E (4*D)

char defs[T][1 << E];

int found[T];

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

int def1(char g[2*N][2*N], int dx, int dy, int x0, int y0) {
	int d = 0;
	int x, y;
	int i;
	int c;
	for (i = N-1; i >= 0; i--) {
		x = x0 + i*dx;
		y = y0 + i*dy;
		c = clue(g, x, y, dy, -dx);
		d <<= 1;
		d |= c == g[y][x];
	}
	i = dx;
	dx = -dy;
	dy = i;
	for (i = 0; i <= N-1; i++) {
		x = x0 + i*dx;
		y = y0 + i*dy;
		c = clue(g, x, y, -dy, dx);
		if (i == 0) {
			if ((c == g[y][x]) ^ ((d&1) == 1)) {
				return -1;
			}
		} else {
			d <<= 1;
			d |= c == g[y][x];
		}
	}
	return d;
}

int def(char g[2*N][2*N]) {
	int d1 = def1(g, -1, 0, N-1, N-1);
	int d2 = def1(g, 0, -1, N, N-1);
	int d3 = def1(g, 1, 0, N, N);
	int d4 = def1(g, 0, 1, N-1, N);
	if (d1 < 0 || d2 < 0 || d3 < 0 || d4 < 0) {
		return -1;
	}
	return (d1 << 3*D) | (d2 << 2*D) | (d3 << D) | d4;
}

void printdef(int d) {
	int i;
	for (i = E-1; i >= 0; i--) {
		printf("%c", (d >> i & 1) ? '*' : '.');
	}
	printf("\n");
}

#define R (4*N*N)

char g[T][2*N][2*N];

void printg(int t) {
	int i, j;
	for (i = 0; i < 2*N; i++) {
		for (j = 0; j < 2*N; j++) {
			printf("%c", g[t][i][j] + '0');
		}
		printf("\n");
	}
}

void solve(int t, int x, int y) {
	if (x == 0 && y == 1) {
		printf("%d\n", t);
	}
	if (y >= 2*N) {
//		printf("found\n");
//		printg();
		found[t]++;
		int d = def(g[t]);
		if (d >= 0) {
//			printdef(d);
			logdef(t, d);
		}
		return;
	}

	int i;
	int cands[N+1];
	for (i = 1; i <= N; i++) {
		cands[i] = 1;
	}
	for (i = x/N*N; i < x; i++) {
		cands[g[t][y][i]] = 0;
	}
	for (i = y/N*N; i < y; i++) {
		cands[g[t][i][x]] = 0;
	}

	int xn, yn;
	xn = x + 1;
	yn = y;
	if (xn >= 2*N) {
		xn = 0;
		yn++;
	}
	for (i = 1; i <= N; i++) {
		if (!cands[i]) {
			continue;
		}
		g[t][y][x] = i;
		solve(t, xn, yn);
	}
}

void handle(int sig) {
	int f = 0;
	int i;
	for (i = 0; i < T; i++) {
		f += found[i];
	}
	fprintf(stderr, "found %d\n", f);
	if (sig == SIGINT) {
		exit(1);
	}
}

void *solve10(void *threadid) {
	long t;
	t = (int)threadid;
	solve(t, 1, 0);
	pthread_exit((void*)t);
}

int main(int argc, char **argv) {
	signal(SIGINT, handle);
	signal(SIGHUP, handle);
	long t;
	pthread_t threads[T];
	pthread_attr_t attr;

	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	for (t = 0; t < T; t++) {
		g[t][0][0] = t + 1;
		pthread_create(&threads[t], &attr, solve10, (void *)t);
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
	pthread_exit(NULL);
}

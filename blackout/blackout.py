def solve(l):
    """assumes a blackout math puzzles in the form '1+2+3==14'"""

    sols = []
    for i in xrange(len(l)):
        if l[i] == '=':
            continue
        for j in xrange(i+1, len(l)):
            if l[j] == '=':
                continue
            e = list(l)
            del e[j]
            del e[i]
            try:
                if eval(''.join(e)) == True:
                    sols.append((i,j))
            except:
                pass
    return sols



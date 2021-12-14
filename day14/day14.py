def step(s, t):
    outlen = len(s) * 2 - 1
    out = [0] * outlen
    for i in range(0, len(s)-1):
        out[i*2] = s[i]
        out[i*2+1] = t[s[i:i+2]]
    out[outlen-1] = s[len(s)-1]
    return out

def runSteps(s, t, n):
    for i in range(0, n):
        s = step(s, t)
        s = "".join(s)
    return s

def getCounts(o):
    d = {}
    for i in o:
        if i in d:
            d[i] = d[i] + 1
        else:
            d[i] = 1
    return d

def getResult(d):
    mi = None
    ma = None
    for k in d:
        if mi is None or d[k] < mi:
            mi = d[k]
        if ma is None or d[k] > ma:
            ma = d[k]

    print(ma - mi)

def combineDicts(ds):
    f = {}
    for d in ds:
        for i in d:
            if i in f:
                f[i] = d[i] + f[i] 
            else:
                f[i] = d[i] 
    return f


with open("input.txt") as f:
    lines = f.read().splitlines()
    start = lines[0]
    transforms = [l.split(" -> ") for l in lines[2:]]
    transforms = {x[0]: x[1] for x in transforms}

out = runSteps(start, transforms, 10)
counts = getCounts(out)
getResult(counts)
###################
start20 = runSteps(start,transforms,20)
pairs = [start20[i:i+2] for i in range(0, len(start20)-1)]
step20ByPair = [(k, runSteps(k, transforms, 20)) for k in transforms]
countsByPair = {s: getCounts(st[:len(st)-1]) for (s,st) in step20ByPair}
pairCounts = [countsByPair[p] for p in pairs]
f = combineDicts(pairCounts)
f[start2[len(start2)-1]] = f[start2[len(start2)-1]] + 1
getResult(f)

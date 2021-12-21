memo = {}

def getNext(p,s,x):
    p = (p + x) % 10
    s = s + (p if p > 0 else 10)
    return (p,s)

def play(p1,p2,s1,s2,isP1):
    x = (p1,p2,s1,s2,isP1)
    if x in memo:
        return memo[x]
    if s1 >= 21:
        return (1,0)
    if s2 >= 21:
        return (0,1)
    
    if isP1:
        (p13,s13) = getNext(p1,s1,3)
        (p14,s14) = getNext(p1,s1,4)
        (p15,s15) = getNext(p1,s1,5)
        (p16,s16) = getNext(p1,s1,6)
        (p17,s17) = getNext(p1,s1,7)
        (p18,s18) = getNext(p1,s1,8)
        (p19,s19) = getNext(p1,s1,9)
        (w13,w23) = play(p13,p2,s13,s2,False)
        (w14,w24) = play(p14,p2,s14,s2,False)
        (w15,w25) = play(p15,p2,s15,s2,False)
        (w16,w26) = play(p16,p2,s16,s2,False)
        (w17,w27) = play(p17,p2,s17,s2,False)
        (w18,w28) = play(p18,p2,s18,s2,False)
        (w19,w29) = play(p19,p2,s19,s2,False)
    else:
        (p23,s23) = getNext(p2,s2,3)
        (p24,s24) = getNext(p2,s2,4)
        (p25,s25) = getNext(p2,s2,5)
        (p26,s26) = getNext(p2,s2,6)
        (p27,s27) = getNext(p2,s2,7)
        (p28,s28) = getNext(p2,s2,8)
        (p29,s29) = getNext(p2,s2,9)
        (w13,w23) = play(p1,p23,s1,s23,True)
        (w14,w24) = play(p1,p24,s1,s24,True)
        (w15,w25) = play(p1,p25,s1,s25,True)
        (w16,w26) = play(p1,p26,s1,s26,True)
        (w17,w27) = play(p1,p27,s1,s27,True)
        (w18,w28) = play(p1,p28,s1,s28,True)
        (w19,w29) = play(p1,p29,s1,s29,True)

    s1n = w13 + 3*w14 + 6*w15 + 7*w16 + 6*w17 + 3*w18 + w19
    s2n = w23 + 3*w24 + 6*w25 + 7*w26 + 6*w27 + 3*w28 + w29
    memo[x] = (s1n,s2n)
    return (s1n,s2n)

(w1,w2) = play(6,4,0,0,True)
print(w1)
print(w2)

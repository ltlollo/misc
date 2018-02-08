def fold(seq, op=lambda x, y: x+y, init=0):
    res, last = [], init
    for e in seq + [init]:
        res.append(op(last, e))
        last = e
    return (res, last)

def scan(seq, op=lambda x, y: x+y, init=0):
    res, last = [], init
    for e in seq:
        res.append(last)       
        last = op(last, e)
    return (res, last)


def repeat(n, init=0): return map(lambda x: init, range(n))

def gather(seq, pos, mask, out, op=lambda x, y: x+y):
    s = out
    for (e, p, m) in zip(seq, pos, mask):
        if m:
            s[p] = op(s[p], e)
    return s


def subseq(seq, fil):
    b = map(fil, seq)
    r, _ = fold(b, lambda x, y : x > y, False)
    l, _ = fold(b, lambda x, y : x < y, False)
    
    mr, s = scan(r)
    ml, s = scan(l)
    iota = range(len(mr))

    ph = gather(iota, ml, l, repeat(s))
    sz = gather(iota, mr, r, repeat(s))

    return (ph, map(lambda x, y: x-y, sz, ph))

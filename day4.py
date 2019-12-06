#100 possibilities per pair naive
#One must have 11*n
#No pair may decrease
start=372037
end=905157
def split(n):
    s=str(n)
    r=[s[:2],s[2:4],s[4:]]
    return list(map(int,r))

def both_the_same(n,debug=False):
    c=1
    cha=None
    for i in range(len(n)):
        if debug:
            print(cha,n[i],c)
        if cha is None or cha!=n[i]:
            if c==2:
                return True
            cha=n[i]
            c=1
        else:
            c+=1
    return c==2

def noDecrease(n):
    for i in range(len(n)-1):
        if int(n[i])>int(n[i+1]):
            return False
    return True
c=0
l=[]
for i in range(start,end):
    s=str(i)
    if both_the_same(s) and noDecrease(s):
        l.append(i)
        c+=1

print(c)

from collections import DefaultDict

with open('day-5.txt') as f:
    c=f.read()
#    c="1,9,10,3,2,3,11,0,99,30,40,50"
    c=list(map(int,c.split(',')))

def mode(n):
    return (n//100)%10,(n//1000)%10,(n//10000)%10

def run_intcode(c):
    d=DefaultDict(
    for i in c:
        d.append(i)
    c=d
    pc=0
    while True:
        #print(f"pc={pc}")
        #print(c[pc:pc+4])
        instr=c[pc]%100;
        if c[pc]==99:
            break
        imm=mode(c[pc])
        a=c[pc+1]
        b=c[pc+2]
        if imm[1]==0:
            b=c[pc+2]
            b=c[b]
        d=c[pc+3]
        if instr==1:
            if imm[0]==0:
                a=c[a]
            c[d]=a+b
            print(f"setting {d} to {a}+{b}")
            pc+=4
        elif instr==2:
            if imm[0]==0:
                a=c[a]
            c[d]=b*a
            #print(f"setting {d} to {a}*{b}")
            pc+=4
        elif instr==3:
            c[a]=int(input("READING>"))
            pc+=2
        elif instr==4:
            if imm[0]==0:
                a=c[a]
            print(c[a])
            pc+=2
        else:
            print(f'error at position {pc}')
            break
    return c


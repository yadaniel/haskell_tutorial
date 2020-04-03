#!/usr/bin/python3

# abcd 
# a = 1..9
# b,c,d = 0..9
# 1000 ... 9999

# str(i)

lst = []
for i in range(1000, 10000):
    (a,b,c,d) = str(i)
    (a,b,c,d) = int(a),int(b),int(c),int(d)
    if b+c > a+d:
        lst.append(i)

print(len(lst))

cntMax = 0
cnt = 0
while len(lst)>=2:
    x,xn,*xall = lst
    lst = xall + [xn]
    if x + 1 == xn:
        cnt += 1
        if cnt > cntMax:
            cntMax = cnt
    else:
        cnt = 0

print(cntMax)
 
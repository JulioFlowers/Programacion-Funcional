def pascal (n):
     if n ==1:
         return [1]
     else :
          r= pascal (n -1)
          x =[0]+ r
          y=r +[0]
          return [i+j for i,j in zip (x,y)]

def nats (n):
     yield n
     yield from nats (n+1)

def binnum (n,mxgrd):
     
     bnumcoff = []

     for i in range(mxgrd+1):
         bnumcoff = bnumcoff + [pow(n,i)]

     return bnumcoff

s= nats (7)
pcoeff = pascal ( next (s))

anum = binnum (2,6)
anum.reverse()
bnum = binnum (1,6)


def represent(pc,an, bn, mxgrd):
     
     print("Expancion de (2x+1)^6: ",end='')

     ptt = mxgrd+1
    
     for i in range(ptt): 
          
          pot = mxgrd-i

          if i < mxgrd  :       
              x = "x^" + str(pot)

              print(str(pc[i]*an[i]*bn[i]) + x + " + " ,end='')

          else:       
              print(str(pc[i]*an[i]*bn[i]))

represent(pcoeff,anum,bnum,6)
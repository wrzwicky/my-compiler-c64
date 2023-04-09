   10 dim w$(50):rem list of words
   20 nw=0:rem # words
   30 gosub 1000 : end
  998 :
  999 :
 1000 read a$
 1010 if a$="*end" then return
 1020 gosub 2000
 1025 gosub 5000
 1030 goto 1000
 1499 :
 1500 data for,to,list,run,plot[,draw,goto,graphic[
 1510 data x,y,color,old.x,old.y,i,*end
 2000 rem find the location for a word
 2001 print a$" ";
 2005 l%=nw/2:ol%=nw
 2010 print".";
 2015 if a$<w$(l%) then nl%=l%/2:goto 2040
 2020 if a$>w$(l%) then nl%=(ol%+l%)/2:goto 2040
 2030 goto 2070
 2040 if l%-nl%<=1 then 2060
 2050 ol%=l%:l%=nl%:goto 2010
 2060 l%=nl%:if a$<w$(l%) then l%=l%-1
 2070 gosub 2100:w$(l%)=a$:print"inserted!"
 2075 stop
 2080 return
 2099 :
 2100 rem insert a space in word list
 2110 if nw=50 then print"word overflow!":return
 2120 nw=nw+1:for ii=nw to l%+1 step -1
 2130 w$(ii)=w$(ii-1):next:return
 5000 for pi=0 to nw:printw$(pi):next
 5010 get a$:if a$="" then 5010
 5020 return

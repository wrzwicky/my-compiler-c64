   10 dim w$(50):rem list of words
   20 nw=0:rem # words
   30 gosub1000
   40 sp$="                "
  100 print:print"{down}ready.":print">";:ll=0
  110 a$="":w$="":l=len(w$)
  115 print"{up}":printtab(ll+1)a$;
  120 poke 204,0:get k$:if k$="" then 120
  130 poke 204,1
  150 if k$="{arrow left}" then print"{arrow left}":end
  160 if k$=chr$(20) then a$=left$(a$,len(a$)-1):goto 190
  161 if k$<>" " and k$<>chr$(13) then 170
  162 print" ";:if peek(1063)=42 then print"{up}":printtab(ll+1)w$" ";
  163 ll=ll+l+1
  164 if peek(1063)=32 and a$<>"" then print:gosub 2500
  165 if k$=" " then 110
  166 goto 100:rem k$=c.r.
  170 if k$="{f1}" then gosub 5000:goto 100
  180 printk$;:a$=a$+k$:e=l-len(a$):if e>0 then print left$(sp$,e);
  190 gosub 2000:if d%>0 then l%=l%+1
  195 w$=w$(l%):l=len(w$)
  200 poke 1063,32
  210 if a$=left$(w$,len(a$)) then print"{up}":printtab(ll+1)w$(l%);:poke 1063,42
  220 print"{up}":printtab(ll+1)a$;:goto 120
  997 end
  998 :
  999 :
 1000 read a$
 1010 if a$="*end" then return
 1020 gosub 2500
 1030 goto 1000
 1499 :
 1500 data for,to,plot,line,goto,graphic
 1510 data color,circle,sound
 1520 data catalog,scratch,format,copy
 1530 data open,close,input,print
 1540 data poke,column,gosub
 1590 data *end
 1998 :
 1999 :
 2000 rem find the location for a word
 2001 rem  l% is loc; d%=-1,0,1 -> which
 2002 rem  side of l% to go to
 2003 for p=1024 to 1030:poke p,32:next:p=1023
 2005 l%=nw/2:lo%=0:hi%=nw
 2010 p=p+1:poke p,46
 2015 if a$<w$(l%) then hi%=l%:l%=(l%+lo%)/2:goto 2040
 2020 if a$>w$(l%) then lo%=l%:l%=(l%+hi%)/2+.5:goto 2040
 2030 goto 2050
 2040 if hi%-lo%>1 then 2010
 2050 d%=0
 2060 if a$>w$(l%) then d%=1
 2070 if a$<w$(l%) then d%=-1
 2080 return
 2099 :
 2100 rem insert a space in word list
 2110 if nw=50 then print"word overflow!":return
 2120 nw=nw+1:for ii=nw-1 to l% step -1
 2130 w$(ii+1)=w$(ii):next:return
 2498 :
 2499 :
 2500 rem insertion sort one word (a$)
 2510 print a$;" ";
 2520 gosub 2000:rem find pos
 2530 if d%=0 then print"exists":goto 2560
 2540 if d%=1 then l%=l%+1
 2550 gosub 2100:w$(l%)=a$:print"inserted!"
 2560 return
 5000 sp$="          ":print:print"{reverse on} words: "
 5010 for p=1 to nw
 5020 printleft$(w$(p)+sp$,10);:next
 5030 return

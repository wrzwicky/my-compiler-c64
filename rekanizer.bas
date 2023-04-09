   10 dim w$(50):rem list of words
   20 nw=0:rem # words
   30 gosub1000
   40 sp$="                "
   50 poke 54296,15:poke 54272,0:poke 54273,75:poke 54277,9:poke 54278,9
  100 print:print"{down}ready.":print">";:ll=0
  110 a$="":w$="":l=len(w$):l%=0
  120 print"{up}":printtab(ll+1)a$;
  130 poke 1024,len(a$):poke 204,0:get k$:if k$="" then 130
  140 poke 204,1
  150 if k$="{arrow left}" then print"{arrow left}":end
  160 if k$=chr$(20) then print "{left} {left}";:a$=left$(a$,abs(len(a$)-1)):goto 250
  170 if k$<>" " and k$<>chr$(13) then 230
  180 if len(a$)>0 and peek(1063)=32 then gosub 4000:goto 130
  190 print"{up}":print">";spc(ll)w$(l%)" ";
  200 ll=ll+l+1
  210 if k$=" " then 110
  220 goto 100:rem k$=c.r.
  230 if k$="{f1}" then gosub 5000:goto 100
  235 if k$="{sh space}" then print:gosub 2500:l=len(a$):k$=" ":goto 190
  240 printk$;:a$=a$+k$
  250 e=l-len(a$):if e>0 then print left$(sp$,e);
  260 gosub 2000:if d%>0 then l%=l%+1
  270 w$=w$(l%):l=len(w$)
  280 poke 1063,32
  290 if a$=left$(w$,len(a$)) then print"{up}":print">";spc(ll)w$(l%);:poke 1063,42
  300 print"{up}":printtab(ll+1)a$;:goto 130
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
 2003 rem p=1024 to 1030:poke p,32:next:p=1023
 2005 l%=nw/2:lo%=0:hi%=nw
 2010 p=p+1:rem  p,46
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
 3996 :
 3997 :
 3998 rem misc routines
 3999 :
 4000 rem bell
 4010 poke 54276,16:poke53280,1
 4020 poke 54276,17:poke53280,14:return
 4099 :
 5000 sp$="          ":print:print"{reverse on} words: "
 5010 for p=1 to nw
 5020 printleft$(w$(p)+sp$,10);:next
 5030 return

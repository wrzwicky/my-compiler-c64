    0 rem rekanizer7
    1 rem wrz 7-11-90
    9 :
   10 dim w$(50),b(50),n(50),bt$(50),nn(50):   rem word,bank,item#;btype,#items/b
   20 nw=0:rem # words
   30 gosub1000
   40 sp$="                "
   50 poke 54296,15:poke 54272,0:poke 54273,75:poke 54277,9:poke 54278,9
   60 dim cm(20):rem this line's tokens
  100 print"{clear}{down}ready.":ll=0
  110 cm$="":cl=0:cm(0)=0:tp(0)=0:rem cmd      text, # tokens, token list,pos
  120 a$="":w$="":l=len(w$):l%=0
  130 print"{up}":printtab(ll)a$;
  140 poke 1024,len(a$):poke 204,0
  150 get k$:if k$="" then 150
  160 poke 204,1
  170 if (k$>"@" and k$<"[") or k$="." then 350:rem alpha or . means word
  180 if k$="{arrow left}" then print"{arrow left}":end
  190 if k$=chr$(20) then print "{left} {left}";:a$=left$(a$,abs(len(a$)-1)):goto 360
  200 if k$<>" " and k$<>chr$(13) then 280
  210 if len(a$)>0 and peek(1063)=32 then gosub 4000:goto 140
  220 print"{up}":printtab(ll)w$(l%)" ";
  230 cm$=cm$+w$+" ":cm(cl)=l%
  240 ll=ll+l+1
  250 cl=cl+1:tp(cl)=ll
  260 if k$=chr$(13) then 100
  270 goto 120:rem k$=anything else
  280 if k$<" " or k$>"@" then 310:rem       possible chars for ops
  290 w$=k$:gosub 2000
  300 if d%=0 then if bt$(b(wl%))="op" then 400:rem k$="op"->new word
  310 if k$="{f1}" then gosub 5000:goto 100
  320 if k$="{ct c}" then 100
  330 if k$<>"{sh space}" then 340:rem sft-spc
  332 print:input "type of item";ty$:gosub 2300
  334 w$=a$:gosub 2500:l=len(a$):goto 220
  340 if k$<" " or k$>"{arrow left}" then 140:            rem asc 32-95 allowed
  350 printk$;:a$=a$+k$
  360 e=l-len(a$):if e>0 then print left$(sp$,e);
  370 w$=a$:gosub 2000:l%=wl%:if d%>0 then l%=l%+1
  380 w$=w$(l%):l=len(w$)
  390 poke 1063,32
  400 if a$=left$(w$,len(a$)) then print"{up}":printtab(ll)w$(l%);:poke 1063,42
  410 print"{up}":printtab(ll)a$;:goto 140
  997 end
  998 :
  999 :
 1000 read w$:if w$="{arrow left}end" then return
 1010 if left$(w$,1)<>"{arrow left}" then gosub 2500:    goto 1000
 1020 ty$=right$(w$,len(w$)-1)
 1030 gosub 2300:rem open bank
 1060 bt$(b)=ty$:nb=nb+1
 1070 goto 1000
 1499 :
 1500 data {arrow left}op,+,-,*,/,^
 1510 data {arrow left}cmd,for,to,goto,gosub,poke
 1520 data  open,close,input,print
 1530 data  format,copy,scratch,catalog
 1540 data  graphic,scnclr,plot
 1550 data  line,circle
 1560 data {arrow left}func,sin,cos,rand,pos,csrpos
 1570 data {arrow left}end
 1590 data {arrow left}end
 1998 :
 1999 :
 2000 rem find the location for a word
 2010 rem  w$=word to find
 2020 rem  wl% is loc; d%=-1,0,1 -> which
 2030 rem  side of l% to go to
 2040 rem p=1024 to 1030:poke p,32:next:p=1023
 2050 wl%=nw/2:lo%=0:hi%=nw
 2060 p=p+1:rem  p,46
 2070 if w$<w$(wl%) then hi%=wl%:wl%=(wl%+lo%)/2:goto 2100
 2080 if w$>w$(wl%) then lo%=wl%:wl%=(wl%+hi%)/2+.5:goto 2100
 2090 goto 2110
 2100 if hi%-lo%>1 then 2060
 2110 d%=0
 2120 if w$>w$(wl%) then d%=1
 2130 if w$<w$(wl%) then d%=-1
 2140 return
 2199 :
 2200 rem insert a space in word list
 2210 if nw=50 then print"word overflow!":return
 2220 nw=nw+1:for ii=nw-1 to wl% step -1
 2230 w$(ii+1)=w$(ii)
 2240 b(ii+1)=b(ii):n(ii+1)=n(ii):next
 2250 return
 2199 :
 2300 rem open a bank
 2310 for b=0 to nb-1
 2320 if ty$<>bt$(b) then next:bt$(b)=ty$:nb=nb+1:goto 2340
 2330 ty$=bt$(b)
 2340 return
 2498 :
 2499 :
 2500 rem insertion sort one word (w$)             into bank b
 2510 print w$;" ";
 2520 gosub 2000:rem find pos
 2530 if d%=0 then print"exists":goto 2560
 2540 if d%=1 then wl%=wl%+1
 2550 gosub 2200:w$(wl%)=w$:b(wl%)=b:n(wl%)=nn(b):nn(b)=nn(b)+1
 2560 print tab(16)"inserted as "ty$
 2570 return
 3996 :
 3997 :
 3998 rem misc routines
 3999 :
 4000 rem bell
 4010 poke 54276,16:poke53280,1
 4020 poke 54276,17:poke53280,14:return
 4099 :
 5000 sp$="          ":print:print"{reverse on} words: "
 5005 w2=int(nw/2+.5)
 5010 for p=1 to w2
 5020 print w$(p);tab(10)bt$(b(p));
 5030 if p+w2<=nw then print tab(20)w$(p+w2);tab(30)bt$(b(p+w2));
 5040 print:next
 5050 print"{reverse on}press a key"
 5060 get a$:if a$="" then 5060
 5070 return

!- parsec.1.34
!- verbose parsing
!- doesn't work, = not implemented at 10380
   10 print "{white}{clear}{down}{down}welcome to zparser compiler{down}"
   15 do=0:po=0:rem no device output
   20 gosub 50000:rem init arrays
   30 gosub 51000:rem init tokens
   39 :
   40 read ne:dim er$(ne):for i=1 to ne
   50 read er$(i):next
   55 rem * data is at ln 63500
   59 :
   80 pe=1:rem print all errors
   90 :
   95 print "{down}ready for formulae."
  100 print "{down}ok."
  110 f$=""
  120 sys 65487:if peek(780)=13 then 135
  130 f$=f$+chr$(peek(780)):goto 120
  135 if f$=" " then print:goto 110
  137 f$=f$+"{arrow left}"
  140 print:print ":{left}";
  150 if left$(f$,4)<>"dump" then 200
  155 for i=1 to nw
  160 : b$=bt$(b(i))
  165 : if left$(b$,1)="*" then print w$(i)      tab(20)right$(b$,len(b$)-1)
  170 : next i
  175 goto 100
  180 :
  200 if left$(f$,4)="quit" then close 8:end
  210 if left$(f$,4)<>"help" then 300
  220 print "available commands:"
  225 print "  decl - declare a variable"
  230 print "  dump - list all variables"
  235 print "  file - open an executable file"
  240 print "  prin - send program to printer"
  250 print "  help - display this"
  260 print "  quit - end program"
  270 goto 100
  290 :
  300 if left$(f$,4)<>"decl" then 390
  305 : l=len(f$)
  310 : for p1=4 to l
  320 :   if mid$(f$,p1,1)<>" " then next
  330 : if p1>l then er=1:l=330:ptr=5:            gosub 60000:goto 100
  332 : for p2=p1+1 to l
  334 :   if mid$(f$,p2,1)<>" " then next
  336 : if p2>l then er=13:l=336:ptr=p2:          gosub 60000:goto 100
  340 : w$ =mid$(f$,p1+1,p2-p1-1):              ty$="*"+mid$(f$,p2+1,l-p2-1)
  350 : gosub 26000:if er<>0 then 100
  355 : gosub 27000:if er<>0 then 100
  360 : rem   "variable '";vr$;"' created as ";nt$
  370 : goto 100
  380 :
  390 if left$(f$,4)<>"file" then 490
  400 : for i=4 to len(f$)
  410 :   if mid$(f$,i,1)<>" " then next
  420 : if i>len(f$) then er=1:l=420:ptr=i       :gosub 60000:goto 100
  430 : n$=mid$(f$,i+1,len(f$)-i-1)
  440 : print "open 8,8,2,"chr$(34);n$",p,w"chr$(34):do=1:open 8,8,2
  450 : pl=0
  460 : l$="dim var(100)":gosub 23000
  470 : goto 100
  480 :
  490 if left$(f$,4)<>"prin" then 900
  500 : if po=0 then po=1:open 4,4               :print "printer on":goto 520
  510 : if po=1 then po=0:close 4                :print "printer off"
  520 : goto 100
  898 :
  899 :
  900 gosub 10000
  910 goto 100
 9970 :
 9980 :
 9990 :
 10000 ptr=0:part=1:sp=1:er=0:pe=1             :es=nv+1
 10050 :
 10060 rem get next char
 10070 ptr=ptr+1
 10080 c$=mid$(f$,ptr,1)
 10085 if c$="{arrow left}" then op$=c$:                   gosub 21000:return
 10090 if c$<"0" or c$>"9" then 10130
 10100 : if part=1 then part=2:goto 20000        :rem # expected
 10110 : if part=2 then op$="*":gosub 21000      :goto 20000:rem implied *
 10120 :
 10130 if c$<"a" or c$>"z" then 10170
 10140 : if part=1 then part=2:gosub 20200       :goto 10060:rem var expctd
 10150 : if part=2 then op$="*":gosub 21000 :gosub 20200:goto 10060:rem implied *
 10160 :
 10170 if c$<>"+" and c$<>"*" and c$<>"/"       and c$<>"^" then 10210
 10180 : if part=1 then er=1:l=10180:gosub        60000:return:rem var needed
 10190 : if part=2 then op$=c$:gosub 21000       :part=1:goto 10060
 10200 :
 10210 if c$<>"-" then 10300
 10220 : if part=2 then op$="-":gosub 21000       :part=1:goto 10060
 10230 : c2$=mid$(f$,ptr+1,1):if c2$="{arrow left}"          then er=2:gosub 60000:return
 10240 : if c2$>="a" and c2$<="z" then            op$="{sh asterisk}":gosub 21000:goto 10060
 10250 : if c2$="(" then op$="{sh asterisk}":gosub            21000:goto 10060
 10260 : if c2$>="0" and c2$<="9" then            part=2:goto 20000:rem # here
 10270 : if c2$="-" then 10060:                   rem 2 minuses cancel
 10280 : er=3:l=10280:gosub 60000:return
 10290 :
 10300 if c$<>"(" then 10340
 10310 : if part=1 then pr=pr+10:goto 10060
 10320 : if part=2 then op$="*":gosub 21000       :pr=pr+10:part=1:goto 10060
 10330 :
 10340 if c$<>")" then 10380
 10350 : if part=1 then er=1:l=10350:gosub        60000:return
 10360 : if part=2 then pr=pr-10:goto 10060
 10370 :
 10380 rem process '='
 10470 rem process ','
 10510 :
 10520 rem what's this char?
 10530 er=3:l=10530:gosub 60000:return
 10540 :
 19980 :
 19990 :
 20000 rem extract constant
 20005 print"extract constant
 20010 vr$=c$:pd=0:ee=0
 20020 ptr=ptr+1:c$=mid$(f$,ptr,1)
 20030 if c$="{arrow left}" then 20120
 20040 if c$=" " then 20020
 20050 if c$>="0" and c$<="9" then              vr$=vr$+c$:goto 20020
 20060 if c$<>"." then 20090
 20070 : if pd=1 then 20120
 20080 : pd=1:vr$=vr$+c$:goto 20020
 20090 if c$<>"e" then 20120
 20100 : if ee=1 then 20120
 20110 : ee=1:vr$=vr$+c$:goto 20020
 20120 ptr=ptr-1:goto 10060
 20130 :
 20140 :
 20200 rem extract variable
 20205 print  "extract variable
 20210 :
 20220 vr$=c$
 20230 ptr=ptr+1:c$=mid$(f$,ptr,1)
 20240 if c$="[" then 20400:rem if any other ops req spcl atn, put them here
 20250 rem i.e. '[' means array
 20260 if c$<"0" or c$>"z" then 20300
 20280 vr$=vr$+c$:goto 20230
 20290 :
 20300 ptr=ptr-1
 20310 pe=0:mk=0:gosub 20500:pe=1
 20320 if er=0 then vr$="var("+str$(vr)+")"    :nt$=vt$(vr):return
 20350 :
 20400 rem
 20410 : for i=1 to nf
 20420 :  rem vr$<>fc$(i) then next:goto            20490
 20430 : nt$="function"
 20440 : return
 20480 :
 20490 er=11:l=20200:return:rem undef'd var
 20498 :
 20499 :
 20500 rem var$->var# :  vr$->vr
 20502 print  "var$->var#
 20505 er=0
 20510 if left$(vr$,1)<"a" or left$(vr$,1)>"z" then print"vr$<>var @20510":return
 20520 if nv=0 then vr=1:goto 20560
 20530 for vr=1 to nv
 20540 if vr$=va$(vr) then return
 20550 next
 20560 if mk=0 then er=11:l=20560:gosub         60000:return
 20570 nv=nv+1
 20580 va$(vr)=vr$:vt$(vr)=nt$
 20590 return
 20620 :
 20630 :
 21000 rem push var,op,priority
 21002 print  "push var,op,priority
 21005 v2$=vr$:n2$=nt$
 21010 op=5    :rem if op$ is a func
 21020 if op$="{arrow left}" then op=0
 21030 if op$="=" then op=1
 21040 if op$="+" or op$="-" then op=2
 21050 if op$="*" or op$="/" then op=3
 21060 if op$="^" then op=4
 21065 op=op+pr
 21070 if op>ps(sp-1) then pv$=vr$:             goto 21210
 21080 tp$=op$:tr=op
 21090 rem
 21100 gosub 22000:v1$=vr$:n1$=nt$
 21110 if v1$="t+" then es=es-1:v1$="var("+str$(es)+")"
 21120 if op$="+" then 41000
 21130 if op$="-" then 42000
 21140 if op$="*" then 43000
 21150 if op$="/" then 44000
 21160 if op$="^" then 45000
 21170 l=21170:er=3:gosub 60000:return
 21180 v2$="t"
 21190 if sp=1 then 21200
 21195 if tr<=ps(sp-1) then 21100
 21200 op$=tp$:op=tr
 21210 if vs$(sp-1)<>"t" then 21220
 21212 vs$(sp-1)="t+"
 21214 l$="var("+str$(es)+")=t":gosub23000:es=es+1
 21220 vs$(sp)=v2$
 21225 os$(sp)=op$
 21230 ps(sp)=op
 21235 ts$(sp)=n2$
 21240 sp=sp+1
 21250 if sp>ss then er=10:l=21250:gosub        60000:return
 21260 return
 21270 :
 22000 rem pull stuff
 22005 print  "pull stuff
 22010 sp=sp-1
 22020 vr$=vs$(sp)
 22030 op$=os$(sp)
 22040 op =pr(sp)
 22045 nt$=ts$(sp)
 22050 return
 22060 :
 22070 :
 23000 rem generate next line number
 23001 rem  and send line to devices
 23010 pl=pl+10
 23020 print pl;l$;tab(25)n1$;tab(32)n2$
 23030 if do=1 then print#8,l$
 23040 if po=1 then print#4,pl;l$
 23050 return
 23998 :
 23999 :
 24000 rem find the location for a word
 24010 rem  w$=word to find
 24020 rem  wl% is loc; d%=-1,0,1 -> which
 24030 rem  side of l% to go to
 24040 rem p=1024 to 1030:poke p,32:next:p=1023
 24050 wl%=nw/2:lo%=0:hi%=nw
 24060 p=p+1:rem  p,46
 24070 if w$<w$(wl%) then hi%=wl%:wl%=(wl%+lo%)/2:goto 24100
 24080 if w$>w$(wl%) then lo%=wl%:wl%=(wl%+hi%)/2+.5:goto 24100
 24090 goto 24110
 24100 if hi%-lo%>1 then 24060
 24110 d%=0
 24120 if w$>w$(wl%) then d%=1
 24130 if w$<w$(wl%) then d%=-1
 24140 return
 24999 :
 25000 rem insert a space in word list
 25010 if nw=50 then print"word overflow!":return
 25020 nw=nw+1:for ii=nw-1 to wl% step -1
 25030 w$(ii+1)=w$(ii)
 25040 b(ii+1)=b(ii):n(ii+1)=n(ii):next
 25050 return
 25999 :
 26000 rem open a bank
 26010 if nb=0 then b=0:goto 26050
 26020 for b=0 to nb-1
 26030 if ty$=bt$(b) then 26060
 26040 next
 26050 bt$(b)=ty$:nb=nb+1:goto 26070
 26060 ty$=bt$(b)
 26070 return
 26999 :
 27000 rem insertion sort one word (w$)             into bank b
 27010 print w$;" ";
 27020 gosub 24000:rem find pos
 27030 if d%=0 then print"exists":goto 27060
 27040 if d%=1 then wl%=wl%+1
 27050 gosub 25000:w$(wl%)=w$:b(wl%)=b:n(wl%)=nn(b):nn(b)=nn(b)+1
 27060 print tab(16)"inserted as "ty$
 27070 return
 27999 :
 29980 :
 29990 :
 41000 l$="t="+v1$+"+"+v2$:gosub 23000
 41900 goto 21180
 41998 :
 41999 :
 42000 l$="t="+v1$+"-"+v2$:gosub 23000
 42900 goto 21180
 42998 :
 42999 :
 43000 l$="t="+v1$+"*"+v2$:gosub 23000
 43900 goto 21180
 43998 :
 43999 :
 44000 l$="t="+v1$+"/"+v2$:gosub 23000
 44900 goto 21180
 44998 :
 44999 :
 45000 l$="t="+v1$+"^"+v2$:gosub 23000
 45900 goto 21180
 50000 rem dimension arrays used
 50010 ss=100:dim vs$(ss),os$(ss),ps(ss),       ts$(ss)
 50015 rem stack: var,op,priority,type
 50020 ps(0)=0:rem force finish a line
 50030 dim va$(100),vt$(100)
 50040 dim an$(20),ad(20),at$(20),al(20):       rem ary name,dim,type,loc
 50050 ts=20:dim w$(ts),b(ts),n(ts):            rem tokens: words,bank #,elem #
 50060 nw=0:rem total number of tokens
 50070 bs=5:dim bt$(bs),nn(bs):                 rem banks: type, # of elem
 50080 nb=0:rem number of banks
 50090 dim param$(bs,ts,5):rem params for       everything
 50092 rem  vars:mem loc
 50100 cm$="":dim cm(20):rem tokenized          command line
 50900 rem er$(ne):rem errors
 50990 return
 50998 :
 50999 :
 51000 rem init built-in tokens
 51010 read w$:if left$(w$,1)<>"{arrow left}" then         gosub 27000:goto 51010
 51020 if w$="{arrow left}end" then return
 51030 ty$=right$(w$,len(w$)-1):gosub 26000
 51040 goto 51010
 51500 rem * data is at ln 63030
 60000 if pe=0 then return
 60001 if er<1 or er>ne then print"bad error # ("er") in"l:return
 60002 print " {left}"tab(ptr-1);"^ ("c$")"
 60003 print "error in line"l":":printer$(er)
 60004 return
 60005 :
 60006 :
 61000 for i=1 to 10
 61001 printvs$(i),os$(i),ps(i):next:end
 63000 rem all data used in this program
 63010 rem  because of no restore # cmd
 63020 :
 63030 rem ops-for ln 51000
 63040 data {arrow left}op,(,),+,-,*,/,^,{arrow left}end
 63499 :
 63500 rem errors-for ln 40
 63510 data 16,missing var,early end of line,syntax,too many '='
 63520 data illegal self operator,equation left of '=',illegal op this side of '='
 63530 data illegal variable name,divide by 0,stack overflow,undefined variable
 63540 data too many close parenthesis,unknown var type,var already exists
 63550 data too many banks,too many tokens in this bank

!- parsec.1.1
!- doesn't work, = not implemented at 10530
   10 print "{clear}{down}{down}welcome to zparser compiler"
   20 ss=100:dim vs$(ss),os$(ss),ps(ss),       nt$(ss)
   25 ps(0)=0
   30 dim va$(100),vt$(100)
   32 dim an$(20),ad(20),av(20,20),at(20):     rem values=av(var #,subscript #)
   39 :
   40 read ne:dim er$(ne):for i=1 to ne
   50 read er$(i):next
   51 data 12,missing var,early end of line,syntax,too many '='
   52 data illegal self operator,equation left of '=',illegal op this side of '='
   53 data illegal variable name,divide by 0,stack overflow,undefined variable
   54 data too many close parenthesis
   59 :
   60 read no:dim op$(no):for i=1 to no
   62 read op$(i):next
   64 op$(1)=","
   66 data 10,,{arrow left},+,-,*,/,^,=,(,)
   69 :
   70 read nf:for i=1 to nf
   72 read fc$(i):next
   74 data 5,sin,cos,tan,atn,exp
   80 pe=1
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
  155 : if nv=0 then print"no vars!":goto 190
  157 print"* scalars:"
  160 : for i=1 to nv
  170 :   print va$(i);tab(20);vt$(i)
  180 :   next
  190 : if na=0 then print "no arrays!":goto 100
  191 : print "* arrays:"
  192 : for i=1 to na
  193 : printan$(i)":":for j=1 to ad(i)
  194 :   print j":"av(i,j):next
  195 : next
  196 : goto 100
  200 if left$(f$,4)="quit" then end
  210 if left$(f$,4)<>"help" then 300
  220 print "available commands:"
  230 print "  decl - declare a variable"
  240 print "  dump - list all variables"
  245 print "  file - open an executable file"
  250 print "  help - display this"
  260 print "  quit - end program"
  270 goto 100
  290 :
  300 if left$(f$,4)<>"decl" then 390
  310 : for i=4 to len(f$)
  320 :   if mid$(f$,i,1)<>" " then next
  330 : if i>len(f$) then er=1:l=300:ptr=i       :gosub 30000:goto 100
  340 : vr$=mid$(f$,i+1,len(f$)-i-1):            nt$="i2"
  350 : mk=1:gosub 20500
  360 : print "variable '";vr$;"' created."
  370 : goto 100
  380 :
  390 if left$(f$,4)<>"file" then 900
  400 : for i=4 to len(f$)
  410 :   if mid$(f$,i,1)<>" " then next
  420 : if i>len(f$) then er=1:l=420:ptr=i       :gosub 30000:goto 100
  430 : n$=mid$(f$,i+1,len(f$)-i-1)
  440 : print "open 1,8,2,"chr$(34);n$",p,w"chr$(34)
  450 : pl=0
  460 : if nv=0 then 490
  470 :   for i=1 to nv
  480 :     gosub 23000:print "rem declare ";        va$(i);" i2":next
  490 : if na=0 then 520
  500 :   for i=1 to na
  510 :     gosub 23000:print "rem declare ";        an$(i);" i2 ["ad(i)"]":next
  520 : gosub 23000:print "rem declare t i2"
  525 : gosub 23000:print"dim stack(100)"
  530 : goto 100
  540 :
  550 :
  900 gosub 10000
  910 goto 100
 9970 :
 9980 :
 9990 :
 10000 ptr=0:part=1:sp=1:er=0:pe=1
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
 10180 : if part=1 then er=1:l=180:gosub          30000:return:rem var needed
 10190 : if part=2 then op$=c$:gosub 21000       :part=1:goto 10060
 10200 :
 10210 if c$<>"-" then 10300
 10220 : if part=2 then op$="-":gosub 21000       :part=1:goto 10060
 10230 : c2$=mid$(f$,ptr+1,1):if c2$="{arrow left}"          then er=2:gosub 30000:return
 10240 : if c2$>="a" and c2$<="z" then            op$="{sh asterisk}":gosub 21000:goto 10060
 10250 : if c2$="(" then op$="{sh asterisk}":gosub            21000:goto 10060
 10260 : if c2$>="0" and c2$<="9" then            part=2:goto 20000:rem # here
 10270 : if c2$="-" then 10060:                   rem 2 minuses cancel
 10280 : er=3:l=280:gosub 30000:return
 10290 :
 10300 if c$<>"(" then 10340
 10310 : if part=1 then pr=pr+10:goto 10060
 10320 : if part=2 then op$="*":gosub 21000       :pr=pr+10:part=1:goto 10060
 10330 :
 10340 if c$<>")" then 10380
 10350 : if part=1 then er=1:l=350:gosub          30000:return
 10360 : if part=2 then pr=pr-10:goto 10060
 10370 :
 10380 rem process '='
 10470 rem process ','
 10510 :
 10520 rem what's this char?
 10530 er=3:l=10530:gosub 30000:return
 10540 :
 19980 :
 19990 :
 20000 rem extract constant
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
 20210 :
 20220 vr$=c$:ep=ptr
 20230 ptr=ptr+1:c$=mid$(f$,ptr,1)
 20240 for i=1 to no:if c$=op$(i) then           20300
 20250 next
 20260 if c$="[" then 20400:rem if any other ops req spcl atn, put them here
 20270 rem i.e. '[' means array
 20280 vr$=vr$+c$:goto 20230
 20290 :
 20300 ptr=ptr-1
 20310 pe=0:mk=0:gosub 20500:pe=1
 20320 if er=0 then vr$=str$(vr)+"Z":           nt$=vt$(vr):return
 20350 :
 20400 rem
 20410 : for i=1 to nf
 20420 :   if vr$<>fc$(i) then next:goto            20490
 20430 : nt$="function"
 20440 : return
 20480 :
 20490 er=11:l=20200:return:rem undef'd var
 20498 :
 20499 :
 20500 rem var$->var# :  vr$->vr
 20505 er=0
 20510 if left$(vr$,1)<"a" or left$(vr$,1)>"z" then print"vr$<>var @20510":return
 20520 if nv=0 then vr=1:goto 20560
 20530 for vr=1 to nv
 20540 if vr$=va$(vr) then return
 20550 next
 20560 if mk=0 then er=11:l=20560:gosub         30000:return
 20570 nv=nv+1
 20580 va$(vr)=vr$:vt$(vr)=nt$
 20590 return
 20620 :
 20630 :
 21000 rem push var,op,priority
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
 21110 if v1$="t+" then v1$="pullZ"
 21120 if op$="+" then 41000
 21130 if op$="-" then 42000
 21140 if op$="*" then 43000
 21150 if op$="/" then 44000
 21160 if op$="^" then 45000
 21170 l=21170:er=3:gosub 30000:stop
 21180 v2$="t"
 21190 if sp=1 then 21200
 21195 if tr<=ps(sp-1) then 21100
 21200 op$=tp$:op=tr:vr$="*"
 21210 if vs$(sp-1)="t" then gosub 23000:       print "push t":vs$(sp-1)="t+"
 21215 vs$(sp)=v2$
 21220 os$(sp)=op$
 21230 ps(sp)=op
 21235 vt$(sp)=n2$
 21240 sp=sp+1
 21250 if sp>ss then er=10:l=21250:gosub        30000:return
 21260 return
 21270 :
 22000 rem pull stuff
 22010 sp=sp-1
 22020 vr$=vs$(sp)
 22030 op$=os$(sp)
 22040 op =pr(sp)
 22045 nt$=vt$(sp)
 22050 return
 22060 :
 22070 :
 23000 rem generate next line number
 23010 pl=pl+10
 23020 print pl;
 23030 return
 23040 :
 23050 :
 29980 :
 29990 :
 30000 if pe=0 then return
 30010 if er<1 or er>ne then print"bad error # ("er") in"l:return
 30020 print " {left}"tab(ptr-1);"^"
 30030 print er$(er);" error in"l
 30040 return
 30050 :
 30060 :
 31000 for i=1 to 10
 31010 printvs$(i),os$(i),ps(i):next:end
 41000 gosub23000:print"t=";v1$;"+";v2$;
 41010 print tab(20);n1$;tab(30);n2$
 41900 goto 21180
 41998 :
 41999 :
 42000 gosub23000:print"t=";v1$;"-";v2$;
 42010 print tab(20);n1$;tab(30);n2$
 42900 goto 21180
 42998 :
 42999 :
 43000 gosub23000:print"t=";v1$;"*";v2$;
 43010 print tab(20);n1$;tab(30);n2$
 43900 goto 21180
 43998 :
 43999 :
 44000 gosub23000:print"t=";v1$;"/";v2$;
 44010 print tab(20);n1$;tab(30);n2$
 44900 goto 21180
 44998 :
 44999 :
 45000 gosub23000:print"t=";v1$;"^";v2$;
 45010 print tab(20);n1$;tab(30);n2$
 45900 goto 21180

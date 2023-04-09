!- parser.2
   10 print "{clear}{down}{down}welcome to zparser"
   20 ss=100:dim vs$(ss),os$(ss),ps(ss)
   30 dim va$(100),vv(ss)
   40 read ne:dim er$(ne):for i=1 to ne
   50 read er$(i):next
   51 data 8,missing var,early end of line,syntax,too many '='
   52 data illegal self operator,equation left of '=',illegal op this side of '='
   53 data illegal variable name
   90 :
   95 print "{down}ready for formulae."
  100 print "{down}ok."
  110 f$=""
  120 sys 65487:f$=f$+chr$(peek(780))
  130 if peek(780)<>13 then 120
  135 if f$=" "+chr$(13) then print:goto 110
  140 print:print ":{left}";
  150 if left$(f$,4)<>"dump" then 200
  160 : for i=1 to nv
  170 :   print va$(i);tab(20);vv(i)
  180 :   next
  190 : goto 100
  200 if left$(f$,4)="quit" then end
  210 gosub 10000
  220 goto 100
 9970 :
 9980 :
 9990 :
 10000 ptr=1:part=1:sp=1:eq=0:er=0
 10010 rem get var to rcve val
 10020 c$=mid$(f$,ptr,1)
 10030 if c$<"a" and c$>"z" then er=8:goto 30000
 10040 gosub 20300:part=2
 10050 :
 10060 rem get next char
 10070 ptr=ptr+1
 10080 c$=mid$(f$,ptr,1)
 10085 if c$=chr$(13) then op$="{arrow left}":gosub 21000    :return
 10090 if c$<"0" or c$>"9" then 10130
 10100 : if part=1 then part=2:goto 20000        :rem # expected
 10110 : if part=2 then op$="*":gosub 21000      :goto 20000:rem implied *
 10120 :
 10130 if c$<"a" or c$>"z" then 10170
 10140 : if part=1 then part=2:goto 20200        :rem var expctd
 10150 : if part=2 then op$="*":gosub 21000      :goto 20200:rem implied *
 10160 :
 10170 if c$<>"+" and c$<>"*" and c$<>"/"       then 10210
 10180 : if part=1 then er=1:l=180:goto 30000    :rem var needed first
 10190 : if part=2 then op$=c$:gosub 21000       :part=1:goto 10060
 10200 :
 10210 if c$<>"-" then 10300
 10220 : if part=2 then op$="-":gosub 21000       :part=1:goto 10060
 10230 : c2$=mid$(f$,ptr+1,1):if c2$=chr$(13)     then er=2:goto 30000:rem eol
 10240 : if c2$>="a" and c2$<="z" then            op$="{sh asterisk}":gosub 21000:goto 10060
 10250 : if c2$="(" then op$="{sh asterisk}":gosub            21000:goto 10060
 10260 : if c2$>="0" and c2$<="9" then            part=2:goto 20000:rem # here
 10270 : if c2$="-" then 10060:                   rem 2 minuses cancel
 10280 : er=3:l=280:goto 30000
 10290 :
 10300 if c$<>"(" then 10340
 10310 : if part=1 then pr=pr+10:goto 10060
 10320 : if part=2 then pr=pr+10:op$="*":         gosub 21000:goto 10060:rem im*
 10330 :
 10340 if c$<>")" then 10380
 10350 : if part=1 then er=1:l=350:goto 30000
 10360 : if part=2 then pr=pr-10:goto 10060
 10370 :
 10380 if c$<>"=" then 10470
 10390 : if eq=1 then er=4:l=390:goto 30000
 10400 : if sp>2 then er=6:l=400:goto 30000
 10410 : if part=2 then op$="=":gosub 21000       :part=1:eq=1:goto 10060
 10420 : gosub 22000:if op$>="a" then         er=5:l=420:goto 30000:rem z exp=1?
 10430 : to$=op$:op$="=":gosub 21000
 10440 : op$=to$:gosub 21000:rem z+ ->            z=z+
 10450 : part=1:eq=1:goto 10060
 10460 :
 10470 if c$<>"," then 10520
 10480 : if eq=1 then er=7:l=480:goto 30000
 10490 : if part=1 then er=3:l=490:goto 30000
 10500 : op$=",":gosub 21000:part=1:goto 10060
 10510 :
 10520 rem what's this char?
 10530 er=3:l=530:goto 30000
 10540 :
 19980 :
 19990 :
 20000 rem extract #
 20010 vr$=c$:pd=0:e=0
 20020 ptr=ptr+1:c$=mid$(f$,ptr,1)
 20030 if c$>="0" and c$<="9" then              vr$=vr$+c$:goto 20020
 20040 if c$<>"." then 20070
 20050 : if pd=1 then 20100
 20060 : pd=1:vr$=vr$+c$:goto 20020
 20070 if c$<>"e" then 20100
 20080 : if e=1 then 20100
 20090 : e =1:vr$=vr$+c$:goto 20020
 20100 ptr=ptr-1:goto 10060
 20110 :
 20120 :
 20200 rem extract variable
 20210 gosub 20300
 20220 goto 10060
 20230 :
 20300 vr$=c$
 20310 ptr=ptr+1:c$=mid$(f$,ptr,1)
 20320 if c$="+" or c$="-" or c$="*" or c$="/" then 20350
 20330 if c$="^" or c$="=" or c$="(" or c$=")" then 20350
 20340 vr$=vr$+c$:goto 20310
 20350 ptr=ptr-1:return
 20360 :
 20370 :
 20380 :
 21000 rem push stuff
 21010 vs$(sp)=vr$
 21020 os$(sp)=op$
 21030 op=5    :rem if op$ is a func
 21040 if op$="=" then op=1
 21050 if op$="+" or op$="-" then op=2
 21060 if op$="*" or op$="/" then op=3
 21070 if op$="^" then op=4
 21080 ps(sp)=op
 21090 sp=sp+1
 21100 return
 29980 :
 29990 :
 30000 if er<1 or er>ne then print"bad error # you fool!! ("er")":goto 100
 30010 print " {left}"tab(ptr-1);"^"
 30020 print er$(er);" error in"l
 30030 goto 100

!- parser1
   10 print "{clear}{down}{down}welcome to zparser"
   20 ss=100:dim vs$(ss),os$(ss),ps(ss)
   30 dim va$(100),vv(ss)
   80 print "{down}ready for formulae."
   90 :
  100 print "{down}ok."
  110 f$=""
  120 sys 65487:f$=f$+chr$(peek(780))
  130 if peek(780)<>13 then 120
  140 print:print ":{left}";
  150 if f$<>"dump" then 200
  160 : for i=1 to nv
  170 :   print va$(i),vv(i)
  180 :   next
  190 : goto 100
  200 if f$="quit" then end
  210 gosub 10000
  220 goto 100
 9970 :
 9980 :
 9990 :
 10000 ptr=1:part=1:sp=1:eq=0:er=0
 10010 rem get var to rcve val
 10020 c$=mid$(f$,ptr,1)
 10030 if c$>="0" and c$<="9" then er=8:goto 30000
 10040 gosub      rem get var name
 10050 :
 10060 rem get next char
 10070 ptr=ptr+1
 10080 c$=mid$(f$,ptr,1)
 10090 if c$<"0" or c$>"9" then 10130
 10100 : if part=1 then part=2:goto 20000        :rem # expected
 10110 : if part=2 then op$="*":gosub 21000      :goto 20000:rem implied *
 10120 :
 10130 if c$<"a" or c$>"z" then 10170
 10140 : if part=1 then part=2:goto 20200        :rem var expctd
 10150 : if part=2 then op$="*":gosub 21000      :goto 20200:rem implied *
 10160 :
 10170 if c$<>"+" and c$<>"*" and c$<>"/"       then 10210
 10180 : if part=1 then er=1:goto 30000          :rem var needed first
 10190 : if part=2 then op$=c$:gosub 21000       :part=1:goto 10060
 10200 :
 10210 if c$<>"-" then
 10220 : if part=2 then op$="-":gosub 21000      :part=1:goto 10060
 10230 :   c2$=mid$(f$,ptr+1,1):if c2$=chr$(13) then er=2:goto 30000:rem eol
 10240 :   if c2$>="a" and c2$<="z" then          op$="{sh asterisk}":gosub 21000:goto 10060
 10250 :   if c2$="(" then op$="{sh asterisk}":gosub          21000:goto 10060
 10260 :   if c2$>="0" and c2$<="9" then          part=2:goto 20000:rem # expctd
 10270 :   if c2$="-" then 10060:                 rem 2 minuses cancel
 10280 :   er=3:goto 30000
 10290 :
 10300 if c$<>"(" then 10340
 10310 : if part=1 then pr=pr+10:goto 10060
 10320 : if part=2 then pr=pr+10:op$="*":         gosub 21000:goto 10060:rem im*
 10330 :
 10340 if c$<>")" then 10380
 10350 : if part=1 then er=1:goto 30000
 10360 : if part=2 then pr=pr-10:goto 10060
 10370 :
 10380 if c$<>"=" then 10470
 10390 : if eq=1 then er=4:goto 30000
 10400 : if sp>2 then er=6:goto 30000
 10410 : if part=2 then op$="=":gosub 21000       :part=1:eq=1:goto 10060
 10420 : gosub 22000:if op$>="a" then             er=5::goto 30000:rem z exp=1?
 10430 : to$=op$:op$="=":gosub 21000
 10440 : op$=to$:gosub 21000:rem z+ ->            z=z+
 10450 : part=1:eq=1:goto 10060
 10460 :
 10470 if c$<>"," then 10520
 10480 : if eq=1 then er=7:goto 30000
 10490 : if part=1 then er=3:goto 30000
 10500 : op$=",":gosub 21000:part=1:goto 10060
 10510 :
 10520 rem what's this char?
 10530 er=3:goto 30000
 10540 :

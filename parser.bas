!- parser.3
   10 print "{clear}{down}{down}welcome to zparser"
   20 ss=100:dim vs$(ss),os$(ss),ps(ss)
   25 ps(0)=0
   30 dim va$(100),vv(100)
   40 read ne:dim er$(ne):for i=1 to ne
   50 read er$(i):next
   51 data 12,missing var,early end of line,syntax,too many '='
   52 data illegal self operator,equation left of '=',illegal op this side of '='
   53 data illegal variable name,divide by 0,stack overflow,undefined variable
   54 data too many close parenthesis
   90 :
   95 print "{down}ready for formulae."
  100 print "{down}ok."
  110 f$=""
  120 sys 65487:f$=f$+chr$(peek(780))
  130 if peek(780)<>13 then 120
  135 if f$=" "+chr$(13) then print:goto 110
  140 print:print ":{left}";
  150 if left$(f$,4)<>"dump" then 200
  155 : if nv=0 then print"no vars!":goto 190
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
 10030 if c$<"a" or c$>"z" then er=8:l=30:goto 30000
 10040 gosub 20300:part=2
 10050 :
 10060 rem get next char
 10070 ptr=ptr+1
 10080 c$=mid$(f$,ptr,1)
 10085 if c$=chr$(13) then op$="{arrow left}":             gosub 21000:return
 10090 if c$<"0" or c$>"9" then 10130
 10100 : if part=1 then part=2:goto 20000        :rem # expected
 10110 : if part=2 then op$="*":gosub 21000      :goto 20000:rem implied *
 10120 :
 10130 if c$<"a" or c$>"z" then 10170
 10140 : if part=1 then part=2:goto 20200        :rem var expctd
 10150 : if part=2 then op$="*":gosub 21000      :goto 20200:rem implied *
 10160 :
 10170 if c$<>"+" and c$<>"*" and c$<>"/"       and c$<>"^" then 10210
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
 10320 : if part=2 then op$="*":gosub 21000       :pr=pr+10:part=1:goto 10060
 10330 :
 10340 if c$<>")" then 10380
 10350 : if part=1 then er=1:l=350:goto 30000
 10360 : if part=2 then pr=pr-10:goto 10060
 10370 :
 10380 if c$<>"=" then 10470
 10390 : if eq=1 then er=4:l=390:goto 30000
 10400 : if sp>2 then er=6:l=400:goto 30000
 10402 : if part=2 and sp>1 then er=6:l=402       :goto 30000
 10410 : if part=2 then op$="=":gosub 21000       :part=1:eq=1:goto 10060
 10420 : gosub 22000:if op$>="a" and op$<="z"     then er=5:l=420:goto 30000
 10430 : tp$=op$:op$="=":gosub 21000
 10440 : op$=tp$:gosub 21000:rem z+ ->            z=z+
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
 20320 if c$="+" or c$="-" or c$="*" or c$="/" or c$="," then 20350
 20330 if c$="^" or c$="=" or c$="(" or c$=")" or c$=chr$(13) then 20350
 20340 vr$=vr$+c$:goto 20310
 20350 ptr=ptr-1:return
 20360 :
 20370 :
 20380 :
 20500 rem val$/var$->val#
 20510 if left$(vr$,1)<"a" or left$(vr$,1)>"z" then vr=val(vr$):return
 20520 if nv=0 then vp=1:goto 20560
 20530 for vp=1 to nv
 20540 if vr$=va$(vp) then 20600
 20550 next
 20560 if mk=0 then er=11:l=20560:goto 30000
 20570 nv=nv+1
 20580 va$(vp)=vr$:vr=0
 20590 return
 20600 vr=vv(vp)
 20610 return
 20620 :
 20630 :
 21000 rem push stuff
 21010 op=5    :rem if op$ is a func
 21020 if op$="{arrow left}" then op=0
 21030 if op$="=" then op=1
 21040 if op$="+" or op$="-" then op=2
 21050 if op$="*" or op$="/" then op=3
 21060 if op$="^" then op=4
 21065 op=op+pr
 21070 if op>ps(sp-1) then 21210
 21080 tp$=op$:tr=op
 21090 mk=0:gosub 20500:v2=vr:t=vr:             rem get val
 21100 gosub 22000:if op$<>"=" then 21115
 21106 : mk=1:gosub 20500
 21109 : vv(vp)=t
 21111 printva$(vp)"="vv(vp)
 21112 : goto 21190
 21115 mk=0:gosub 20500:v1=vr
 21120 if op$="+" then t=v1+v2
 21130 if op$="-" then t=v1-v2
 21140 if op$="*" then t=v1*v2
 21150 if op$<>"/" then 21180
 21160 : if v2=0 then er=9:l=21160:goto 30000
 21170 : t=v1/v2
 21180 if op$="^" then t=v1^v2
 21190 if sp=0 then return
 21195 if tr<=ps(sp-1) then v2=t:goto 21100
 21200 vr=t:vr$=str$(vr):op$=tp$:op=tr
 21210 vs$(sp)=vr$
 21220 os$(sp)=op$
 21230 ps(sp)=op
 21240 sp=sp+1
 21250 if sp>ss then er=10:l=21250:goto 30000
 21260 return
 21270 :
 22000 rem pull stuff
 22010 sp=sp-1
 22020 vr$=vs$(sp)
 22030 op$=os$(sp)
 22040 py =pr(sp)
 22050 return
 22060 :
 22070 :
 29980 :
 29990 :
 30000 if er<1 or er>ne then print"bad error # you fool!! ("er")":goto 100
 30010 print " {left}"tab(ptr-1);"^"
 30020 print er$(er);" error in"l
 30030 goto 100
 30040 :
 30050 :
 31000 for i=1 to 10
 31010 printvs$(i),os$(i),ps(i):next:end

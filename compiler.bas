!- compiler.8.2.2
!- run for test; run 1000 for compiler; run 50000 for linker
0 rem compiler.8.2.2, 24 jul 1991
1 rem  by bill zwicky
2 rem -contains parsec 2.02 and
3 rem  compiler 8.0
4 rem -compiler is to use parsec's
5 rem  token routines.
9 :
10 print "{clear}{down*2}welcome to z compiler, v8.0{down}"
15 fo=0:po=0:rem no device output
20 gosub 30000:rem init arrays
30 rem (init tokens)
32 rem  1000    :rem run compiler
38 :
39 restore 39120
40 read ne:dim em$(ne):for i=1 to ne
50 read em$(i):next
55 rem * data is at ln 63500
59 :
80 pe=1:rem print all errors
82 :
89 rem open bank for exp. eval. stack
90 ty$="math.stack":gosub 26000
92 if en<>0 then print "can't open system stack!" :goto 9000
94 mb=b :rem record bank for math stack
96 :
98 print "{down}ready for formulae."
100 print "{down}ok."
110 f$=""
120 input f$
135 if f$="" then 110
137 f$=f$+"{arrow left}"
140 print ":{left}";
149 :
150 if left$(f$,4)<>"dump" then 200
155 for i=1 to nw
160 : b$=bt$(b(i))
165 : if left$(b$,4)="var-" then print w$(i) tab(20) right$(b$,len(b$)-4)
170 : next i
175 goto 100
180 :
200 if left$(f$,4)="quit" then close 8:end
210 if left$(f$,4)<>"help" then 300
220 print "available commands:"
225 print "{space*2}decl - declare a variable"
230 print "{space*2}dump - list all variables"
235 print "{space*2}file - open an object file"
240 print "{space*2}prin - send program to printer"
250 print "{space*2}help - display this"
260 print "{space*2}quit - end program"
270 goto 100
290 :
300 if left$(f$,4)<>"decl" then 390
305 : l=len(f$)
310 : for p1=4 to l
320 :   if mid$(f$,p1,1)<>" " then next
330 : if p1>l then en=1:l=330:ptr=5:            gosub 39000:goto 100
332 : for p2=p1+1 to l
334 :   if mid$(f$,p2,1)<>" " then next
336 : if p2>l then en=13:l=336:ptr=p2:          gosub 39000:goto 100
340 : w$=mid$(f$,p1+1,p2-p1-1) : ty$=mid$(f$,p2+1,l-p2-1)
342 : rem determine byte length of type ty$
344 : t$=left$(ty$,1) : if t$="i" or t$="x" then tl=2 : else tl=5
346 : ty$="var-"+ty$
350 : gosub 26000:if en<>0 then 100
355 : gosub 27000:if en<>0 then 100
360 : rem   "variable '";vr$;"' created as ";nt$
362 print "(create var:";195;wl%;tl;")"
370 : goto 100
380 :
390 if left$(f$,4)<>"file" then 490
400 : for i=4 to len(f$)
410 :   if mid$(f$,i,1)<>" " then next
420 : if i>len(f$) then en=1:l=420:ptr=i       :gosub 39000:goto 100
430 : n$=mid$(f$,i+1,len(f$)-i-1)
440 : print "open 8,8,2,"chr$(34);n$",p,w"chr$(34):fo=1:open 8,8,2
450 : pl=0
460 : l$="dim var("+str$(bs)+",100)":gosub 23000
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
1000 gosub 5000
1005 input "filename (.fig is added)? test.fig{left*10}";f$
1010 if right$(f$,4)=".fig" then f$=left$(f$,len(f$)-4)
1015 scratch (f$+".obj")
1020 open 1,8,2,f$+".fig,s,r"
1030 of=2:open of,8,3,f$+".obj,u,w"
1040 print#of,chr$(64)chr$(196);"code";chr$(0); :rem select and name code seg
1050 :
1060 cs = -1 :rem current segment
1099 :
1100 input#1,l$ :print l$ :s1=st :if l$="" then begin :if s1=0 then 1100               :else 1900 :bend
1101 p=1 : l=len(l$)
1102 if p <= l then  if mid$(l$,p,1) <> " " then p=p+1 :goto 1102
1104 w$ = left$(l$,p-1) : if p < l then p$=right$(l$,l-p) :else p$ = ""
1110 gosub 24000 :rem find word
1115 print "{space*2}word #";wl%
1120 if p$ <> "" then gosub 31000 :rem compile params
1130 s=0 : m$=dc$(dr(wl%)) : gosub 34040 :rem write data
1140 if s1=0 then 1100
1190 :
1200 s=0 : m$ = chr$(96)
1210 gosub 34040
1899 :
1900 close of:close 1
1910 end
1998 :
1999 :
5000 restore 6000
5010 ty$="func":gosub 26000
5020 read w$:if w$="{arrow left}end" then 5080
5030 gosub 27000:if en<>0 then 39000
5040 dr(wl%)=nd:read dp$(nd)
5050 read v$:if v$<>"{arrow left}" then dc$(nd)=dc$(nd)+chr$(dec(v$)):goto 5050
5060 nd=nd+1:goto 5020
5080 return
6000 data open,"i1 @ a,i1 @ x,i1 @ y"
6005 data 20,ba,ff, a9,00,20,bd,ff
6010 data 20,c0,ff,{arrow left}
6020 :
6030 data start,"i1 @ x"
6035 data 20,c9,ff,{arrow left}
6040 :
6050 data hello,""
6055 data a9,48,20,d2,ff,a9,45,20
6060 data d2,ff,a9,4c,20,d2,ff,a9
6070 data 4c,20,d2,ff,a9,4f,20,d2
6080 data ff,a9,0d,20,d2,ff,{arrow left}
6090 :
6100 data stop,""
6105 data 20,cc,ff,{arrow left}
6110 :
6120 data close,"i1 @ a"
6125 data 20,c3,ff,{arrow left}
6130 :
6140 data {arrow left}end
8999 :
9000 close of
9010 close 1
9090 end
9997 :
9998 :
9999 :
10000 ptr=0:part=1:sp=1:en=0:pe=1:ms=0
10050 :
10060 rem get next char
10070 ptr=ptr+1
10080 c$=mid$(f$,ptr,1)
10082 if c$=" " then 10060
10085 if c$="{arrow left}" then op$=c$:                   gosub 21000:return
10090 if c$<"0" or c$>"9" then 10130
10100 : if part=1 then part=2:goto 20000        :rem extract #
10110 : if part=2 then op$="*":gosub 21000      :goto 20000:rem implied *
10120 :
10130 if c$<"a" or c$>"z" then 10170
10140 : if part=2 then op$="*":gosub 21000      :rem implied mult
10145 : part=2
10150 : gosub 20200 :if en<>0 then return       :rem extract a var
10155 : goto 10060
10160 :
10170 if c$<>"+"and c$<>"*"and c$<>"/"and c$<>"^"and c$<>"="andc$<>","then 10210
10180 : if part=1 then en=1:l=10180:gosub        39000:return:rem var needed
10190 : if part=2 then op$=c$:gosub 21000       :part=1:goto 10060
10200 :
10210 if c$<>"-" then 10300
10220 : if part=2 then op$="-":gosub 21000       :part=1:goto 10060
10230 : op$="{sh asterisk}":gosub 21000:goto 10060
10290 :
10300 if c$<>"(" then 10340
10310 : if part=1 then pr=pr+10:goto 10060
10320 : if part=2 then op$="*":gosub 21000       :pr=pr+10:part=1:goto 10060
10330 :
10340 if c$<>")" then 10380
10350 : if part=1 then en=1:l=10350:gosub        39000:return
10355 : rem else ...
10360 :  if pr>10 then pr=pr-10
10365 :  goto 10060
10370 :
10380 rem anything else to process?
10510 :
10520 rem what's this char?
10530 en=3:l=10530:gosub 39000:return
10540 :
19980 :
19990 :
20000 rem extract constant
20010 vr$=c$:pd=0:ee=0:nt$="i"
20020 ptr=ptr+1:c$=mid$(f$,ptr,1)
20030 if c$="{arrow left}" then 20120
20040 if c$=" " then 20120
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
20220 en=0 : w$=c$
20230 ptr=ptr+1 : c$=mid$(f$,ptr,1)
20260 if c$<"0" or (c$>"9" and c$<"a") or c$>"z" and c$<>"." then 20300
20280 w$=w$+c$ : goto 20230
20290 :
20300 ptr=ptr-1
20310 gosub 24000
20320 if d%<>0 then en=11:l=20200              :goto 39000 :rem undef'd var
20330 vr$="var(" + str$(b(wl%)) + "," +  str$(n(wl%)) + ")"
20340 nt$=bt$(b(wl%))
20350 return
20498 :
20499 :
21000 rem push var,op,priority
21005 v2$=vr$:n2$=nt$
21010 op=5    :rem if op$ is a func
21020 if op$="{arrow left}" then op=0:pr=0                :rem force end of expression
21030 if op$="=" then op=1
21040 if op$="+" or op$="-" then op=2
21050 if op$="*" or op$="/" then op=3
21060 if op$="^" then op=4
21065 op=op+pr
21070 if op>ps(sp-1) then pv$=vr$:             goto 21210
21080 tp$=op$:tr=op
21090 :
21100 if v2$="t" then v2$="("+str$(mb)+","+str$(ms)+")"
21102 gosub 22000:v1$=vr$:n1$=nt$
21105 if v1$="t" then v1$="("+str$(mb)+","+str$(ms)+")"
21110 if v1$="t+" then ms=ms-1:v1$="var(" + str$(mb) + "," + str$(ms) + ")"
21114 rem define accumulator t
21115 vt$="("+str$(mb)+","+str$(ms)+")"
21120 if op$="+" then 41000
21130 if op$="-" then 42000
21140 if op$="*" then 43000
21150 if op$="/" then 44000
21160 if op$="^" then 45000
21165 if op$="=" then 46000
21170 l=21170:en=3:gosub 39000:return
21180 v2$="t"
21190 if sp=1 then 21200
21195 if tr<=ps(sp-1) then 21100
21200 op$=tp$:op=tr
21210 if vs$(sp-1)<>"t" then 21220
21212 vs$(sp-1)="t+"
21213 rem actual pushes not needed; t is now top of stack
21214 rem l$="var(" + str$(mb) + "," + str$(ms) + ")=t" :gosub 23000
21216 ms=ms+1 :if ms>mm then mm=ms
21220 vs$(sp)=v2$
21225 os$(sp)=op$
21230 ps(sp)=op
21235 ts$(sp)=n2$
21240 sp=sp+1
21250 if sp>ss then en=10:l=21250:gosub        39000:return
21260 return
21270 :
22000 rem pull stuff
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
23020 print pl;l$;tab(40)n1$;tab(52)n2$
23030 if fo=1 then print#8,pl;l$
23040 if po=1 then print#4,pl;l$
23050 return
23998 :
23999 :
24000 rem find the location for a word
24010 rem  w$=word to find
24020 rem  wl% is loc; d%=-1,0,1 -> which
24030 rem  side of wl% to go to
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
25040 b(ii+1)=b(ii):n(ii+1)=n(ii):dr(ii+1)=dr(ii):next
25050 return
25999 :
26000 rem open a bank and select it
26005 en=0
26010 if nb=0 then b=0:goto 26050
26020 for b=0 to nb-1
26030 : if ty$=bt$(b) then print "<<select bank:";64+b;")":return
26040 : next
26042 rem create new bank, if space permits
26045 if b>bs then en=15:l=26045:              gosub 39000:goto 27070
26050 nb=nb+1 : bt$(b)=ty$ : tl(b)=tl
26060 print "<<new bank:";64+b;196;ty$;0;")"
26070 return
26999 :
27000 rem insertion sort one word (w$)         into bank b
27005 en=0
27010 print w$;" ";
27020 gosub 24000:rem find pos
27030 if d%=0 then en=14:l=27030:gosub 39000:goto 27070
27040 if d%=1 then wl%=wl%+1
27050 gosub 25000:w$(wl%)=w$:b(wl%)=b:n(wl%)=nn(b):nn(b)=nn(b)+1:dr(wl%)=-1
27060 printtab(16)"inserted as ";ty$
27070 return
27999 :
29980 :
29990 :
30000 rem dimension arrays used
30010 ss=100:dim vs$(ss),os$(ss),ps(ss),       ts$(ss)
30020 rem stack: var,op,priority,type
30030 ps(0)=0:rem force finish a line
30040 :
30050 dim an$(20),ad(20),at$(20),al(20)       :rem ary name,dim,type,loc
30060 ts=20:dim w$(ts),b(ts),n(ts),dr(ts)     :rem tokens: words,bank #,elem #
30070 nw=0:rem total number of words                     : and data ref #
30080 dim dp(ts),dc(ts)                       :rem params and code for words
30090 nd=0                                    :rem # data items
30100 bs=5:dim bt$(bs),nn(bs)                 :rem banks: type, # of elem
30110 nb=0:rem number of banks
30120 dim param$(bs,ts,5)                     :rem params for everything
30130 rem  vars:mem loc
30140 cm$="":dim cm(20):rem tokenized          command line
30150 rem em$(ne):rem errors
30160 return
30170 :
30180 :
30190 :
30200 :
31000 rem compile parameters
31010 rem  wl% = index of parameter template (word number)
31020 rem  p$  = string to be compiled
31030 w=dr(wl%):if dp$(w) = "" then return
31040 it=0 : ip=0 : m$=""
31050 print " (what var type?) ";
31060 : s$=""
31070 : it=it+1:v$=mid$(dp$(w), it, 1)
31080 : it=it+1:c$=mid$(dp$(w), it, 1)
31090 : if c$>="0" and c$<="9" then s$=s$+c$:goto 31080
31100 : print" var type: ";v$;" - ";
31110 :
31120 : if v$="i" then gosub 32000:goto 31160
31130 : if v$="s" then gosub 33000:goto 31160
31140 : print "unknown": goto 9000
31150 :
31160 if it>1 and ip>1 then 31050
31170 :
31180 s=0 : gosub 34040 :rem select data, insert it
31190 return
31200 :
32000 if s$="" then print"missing int size": goto 32500
32010 s=val(s$):if s<1 or s>4 then print"unsupported int size":goto 32500
32020 :
32030 n$="" : print " (fetch the number) ";
32040 ip=ip+1:c$=mid$(p$,ip,1):if c$=" " then 32040   :rem strip leading spcs
32050 if c$<"0" or c$>"9" then 32070
32060 n$=n$+c$ : ip=ip+1 : c$=mid$(p$, ip, 1) : goto 32050
32070 n=val(n$):n$=""
32080 :
32090 print " (convert number to int) ";
32100 for i=1 to s
32110 : n=n/256:n$=n$+chr$( (n - int(n)) * 256 + .2 ):n=int(n)
32120 : next i
32130 :
32140 it=it+1:if mid$(dp$(w), it, 1) <> "@" then print"unknown template command":goto 32500
32150 it=it+1
32160 :
32170 print " (fetch storage location) ";
32180 l$="":lt=0 :rem lt=loc type: 0=dec ad, 1=hex ad, 2=reg
32190 it=it+1:c$=mid$(dp$(w), it, 1)
32200 : if instr("0123456789", c$) > 0  then   l$=l$+c$:goto 32190
32210 : if c$="$" then                         lt=1:goto 32190
32220 : if lt=1 and instr("abcdef", c$) > 0  then l$=l$+c$:goto 32190
32230 : if instr("axy", c$) > 0  then lt=2    :l$=l$+c$:goto 32190
32240 if lt=2 then 32380: rem handle register storage
32250 if lt=1 then l=dec(right$(l$,len(l$)-1))
32260 if lt=0 then l=val(l$)
32270 :
32280 print " (generate code to store the bytes) ";
32290 for i=0 to s-1
32300 : m$ = m$ + chr$(169)+mid$(n$,i+1,1)
32310 : lh=int(l/256) :ll=l-lh*256
32320 : m$ = m$ + chr$(141)+chr$(ll)+chr$(lh)
32330 : l = l+1
32340 : next
32350 :
32360 goto 32500
32370 :
32380 if len(l$) <> len(n$) then print "number of regs doesn't match specified int size":goto 32500
32390 :
32400 rem set up ld? instructions
32410 ld(1)=169:ld(2)=162:ld(3)=160
32420 :
32430 print " (load reg(s)) ";
32440 for i=1 to len(l$)
32450 : m$ = m$ + chr$(ld(instr("axy",mid$(l$,i,1)))) + mid$(n$,i,1)
32460 : next
32470 :
32480 goto 32500
32490 :
32500 print " (advance indices to next ',') ";
32510 it=instr(dp$(w), ",", it)
32520 ip=instr(p$, ".", ip)
32530 :
32540 print
32550 return
32560 :
33000 return :rem strings not implemented yet
34000 rem generate 'write data' linker command
34010 rem   s  = segment to insert data into
34020 rem   m$ = data to write
34030 :
34040 if s <> cs then cs = s :print#of,chr$(64+s);
34050 rem if s>63, use 'quick select'
34060 rem if len(m$) > 63 then <use long form of write, if it exists>
34070 print#of,chr$(len(m$));m$; :rem insert byte count & data
34080 return
39000 rem report errors
39005 if pe=0 then return
39010 if en<1 or en>ne then print"bad error # ("en") in"l:return
39020 if ptr>0 then print " {left}"tab(ptr-1);"^ ("c$")";
39030 print
39040 print "error in line"l"{left}:":printem$(en)
39050 return
39060 :
39070 :
39080 rem all data used in this program
39090 rem  because of no restore # cmd
39100 :
39110 rem errors-for ln 40
39120 data 17,missing var,early end of line,syntax,too many '='
39130 data illegal self operator,equation left of '=',illegal op this side of '='
39140 data illegal variable name,divide by 0,stack overflow,undefined variable
39150 data too many close parenthesis,unknown var type,var already exists
39160 data too many banks,too many tokens in this bank,assignment to a constant
41000 l$=vt$+"="+v1$+"+"+v2$:gosub 23000
41900 goto 21180
41998 :
41999 :
42000 l$=vt$+"="+v1$+"-"+v2$:gosub 23000
42900 goto 21180
42998 :
42999 :
43000 l$=vt$+"="+v1$+"*"+v2$:gosub 23000
43900 goto 21180
43998 :
43999 :
44000 l$=vt$+"="+v1$+"/"+v2$:gosub 23000
44900 goto 21180
44998 :
44999 :
45000 l$=vt$+"="+v1$+"^"+v2$:gosub 23000
45900 goto 21180
45998 :
45999 :
46000 if left$(n1$,4)<>"var-" then en=17:l= 46000:goto 39000 :rem asn to const
46010 l$=v1$+"="+v2$:gosub 23000
46900 goto 21180
46998 :
46999 :
50000 rem fig linker
50001 :
50004 open 5,8,15,"i":close 5:if ds<>0 then printds$:goto 59100
50010 f$="test"
50012 print ".obj file to link [";f$;"]";:input f$
50014 if right$(f$,4) <> ".obj" then i$=f$+".obj": else i$=f$:                       f$=left$(i$,len(i$)-4)
50018 open 2,8,2,i$:close 2:if ds<>0 then print ds$:goto 50005
50020 o$=f$+".ml"
50021 print "executable name [";o$;"]";:input o$
50023 open 2,8,2,o$:close 2:if ds=0 then print "-scratching file":scratch (o$)
50025 print "link: in ";i$;" and out ";o$ : print
50030 :
50040 rem --- general inits ---
50050 z$=chr$(0) : mb = 10 : rem max banks
50060 dim sl(mb,20) : rem symbol location
50070 dim gt$(mb),ga(mb),gs(mb) : rem segment titles, addresses, size (bytes)
50080 dim gl$(mb) : rem segment length (words)
50090 : tg=0 : rem highest (top) seg#
50098 :
50099 :
50100 for ps = 1 to 2
50110 :
50120 rem --- pass 1 inits ---
50125 if ps=1 then begin
50130 : zl = 1 : rem # times to do pass 1
50140 : bend
50145 :
50150 rem --- pass 2 inits ---
50155 if ps=2 then begin
50158 : gosub 61000  :rem resolve segs and syms
50160 : open 3,8,3,o$+",p,w":if ds <> 0 then printds$:goto 59100
50161 : b=int(ga(0)/256):print#3,chr$(ga(0)-256*b);chr$(b);      :rem load address
50162 : zl = tg+1 : rem # times to do pass 2
50163 : ad = ga(0)
50165 : bend
50170 :
50172 for zi=1 to zl : print "working on segment";zi-1;"{down}"
50173 : rem pad distance between segs only if this next one is not empty
50174 : if ps = 2  and  gs(zi-1) > 0  and  ga(zi-1) > ad then                             for i=1 to ga(zi-1)-ad : print#3,z$; : next : ad=ga(zi-1)
50178 : open 2,8,2,i$+",r":if ds <> 0 then printds$:goto 59100
50179 :
50180 get#2,c$ :h=asc(c$+z$) :s2=st :rem chunk header
50190 if (h and 192) = 0   then gosub 51000 : goto 50260 : rem fetch data
50200 if (h and 192) = 64  then gosub 52000 : goto 50260 : rem set segment
50210 if (h and 192) = 128 then gosub 53000 : goto 50260 : rem unused
50220 :
50230 rem --- command handler ---
50240 h = h and 63
50245 if (h and 32) = 32 then gosub 55000 : goto 50260
50250 on h gosub 54000, 54100, 54200, 54300
50260 if s2 = 0 then 50180
50270 :
50280 goto 59000
50998 :
50999 :
51000 rem handle just data
51010 print "data: segment"; sg; ";";
51020 b = h and 63
51025 print b;" bytes"
51030 gs(sg) = gs(sg) + b
51040 if ps = 2  and  sg = zi - 1  then 51500
51050 for i=1 to b  : rem skip the data
51060 : get#2,c$:next
51070 return
51498 :
51499 rem tranfer data to executable (pass 2)
51500 : for i=1 to b  :rem transfer the data
51510 :   get#2,c$:print#3,left$(c$+z$,1);:next
51520 : bend
51530 return
51998 :
51999 :
52000 rem set current segment (pass 1 and 2)
52010 sg = h and 63
52020 print "current segment is now"; sg
52030 if sg>tg then tg=sg
52040 return
52399 :
52998 :
52999 :
53000 rem unused command range
53010 print "nonsense command (";h;") in file!"
53020 goto 59000
53998 :
53999 :
54000 rem define symbol = value
54010 gosub 60000 : s=b
54020 gosub 60100
54025 print "define symbol (";sg;",";s;") at address"; b
54028 if ps=1 then begin
54030 : if sg > tg then tg = sg
54032 : if gl(sg) <= s then gl(sg) = s+1
54034 : ga(sg) = -1   : rem flag as bank of constants
54036 : sl(sg, s) = b
54038 : bend
54040 return
54099 :
54100 rem force segment address
54110 gosub 60100
54115 print "put segment"; sg; " at address"; b
54118 if ps=1 then begin
54120 : ga(sg) = b
54128 : bend
54130 return
54199 :
54200 rem put symbol in segment
54210 gosub 60000 : s=b    :rem symbol #
54220 gosub 60100          :rem # bytes
54225 print "create symbol"; s; " in segment"; sg; ", offset"; gs(sg); ",";
54227 : print b; " byte";:if b<>1 then print"s": else print
54228 if ps=1 then begin
54230 : if gl(sg)<=s then gl(sg)=s+1
54232 : sl(sg, s) = gs(sg) : gs(sg) = gs(sg) + b
54234 : bend : else begin                       :rem reserve space for var
54236 :   if sg = zi-1 and b > 0 then  for i=1 to b:print#3,z$;:next
54238 :   bend
54240 return
54299 :
54300 rem name segment
54310 print "title of segment";sg;" is: ";
54320 t$=""
54330 get#2,c$:if c$<>"" then t$=t$+c$:print c$;:goto 54330
54340 print
54350 if ps = 1 then begin
54360 : st$(sg) = t$
54365 : bend
54370 return
54399 :
55000 rem insert symbol into executable
55010 l = h and 15 : d = h and 16
55020 gosub 60000 : g=b : gosub 60000 : s=b : gosub 60100 : o=b
55030 print "insert symbol ("; g;",";s; ") here as"; l; "bytes, ";
55040 if d=0 then print "low"; : else print "high";
55050 print " byte first"
55055 if s>=gl(g) then print "{ct o}{space*3}*** symbol is not defined ***":return                :rem count error
55060 if ps=2 and sg=zi-1 then begin    :rem do the insertion if at right seg
55062 : v=sl(g,s) + o                   :rem  (get val to ins)
55065 : if d=1 then begin               :rem  high byte first
55070 :   l=l-1 : d=256^int(log(v)/log(2)/8+.99)    :rem min # bytes
55080 :   for i=l to 0 step -1
55085 :     c=int(v/d) : v=v-b*d : d=d/256
55090 :     print#3,chr$(c);
55100 :     next
55110 :  bend : else begin                      :rem low byte first
55120 :   l=l-1
55130 :   for i=0 to l
55140 :     v=v/256 : c=(v-int(v))*256 : v=int(v)
55150 :     print#3,chr$(c);
55160 :     next
55170 :   bend
55180 : bend
55190 return
55998 :
55999 :
59000 print "{down}-one run of pass";ps;"done-{down}"
59010 close 2                 :rem close input file
59015 if ps=2 then ad = ad+gs(zi-1)
59020 next zi                 :rem repeat this pass
59025 print "{reverse on}W{space*2}pass";ps;"{left} done W"
59030 if ps=2 then close 3    :rem close output file (pass 2 only)
59040 next ps                 :rem do next pass
59050 end
59099 :
59100 rem emergency stop
59110 print "emergency abort!"
59120 close 3:close 2
59130 end
59999 :
60000 rem get next byte into c1$, b = value
60010 get#2,c1$ : c1$=left$(c1$+z$,1)
60020 b = asc(c1$)
60030 return
60099 :
60100 rem get next 2 bytes into c1$,c2$ (in that order) b = lo/hi value
60110 get#2,c1$,c2$
60120 c1$=left$(c1$+z$,1) : c2$=left$(c2$+z$,1)
60130 b=asc(c2$) * 256 + asc(c1$)
60140 return
60199 :
60997 :
60998 rem resolver - locate all segments and symbols
60999 :
61000 print "{down*2}resolver"
61020 ad=ga(0)
61030 if ad=0 then ad=49152
61040 print "base address of program [";ad;"]";:input ad
61090 :
61100 print "{down}resolving segments and symbols ..."
61110 for g=0 to tg
61112 : print "seg";g;
61115 : if ga(g) < 0 then print "already resolved" : else begin
61120 :   if ga(g)=0 then ga(g)=ad : ad=ad+gs(g)
61125 :   print "@";ga(g)
61130 :
61135 :   if gl(g) > 0 then begin
61140 :     for s=0 to gl(g)-1
61150 :       sl(g,s) = sl(g,s) + ga(g)
61160 :       print "{space*2}sym";s;" @";sl(g,s)
61170 :       next
61175 :     bend
61180 :   bend
61190 : next
61200 :
61280 print "resolver is done{down*2}"
61290 return
61999 :
62000 open 1,8,2,"test.fig"
62010 get#1,a$:printa$;:if st=0 then 62010
62020 close 1
62030 end
62200 open 1,8,2,"test.obj":z$=chr$(0)
62210 get#1,a$:printright$(hex$(asc(a$+z$)),2)", ";:if st=0 then 62210
62220 close 1
62230 end

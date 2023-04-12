!- compiler.8.3.4
0 rem compiler.8.3.4, 20 jan 1992
1 rem  by bill zwicky
2 rem -contains parsec 2.02 and compiler 8.0
3 rem -compiler uses parsec's token routines
9 :
10 print "{clear}{down}"tab(23)"{reverse on}{cm d}{cm i*29}{cm f}"
11 print tab(23)"{cm k} welcome to z compiler, v8.3 {reverse on}{cm k}"
12 print tab(23)"{reverse on}{cm c}{reverse off}{cm i*29}{reverse on}{cm v}{down}"
15 fo=0:po=0:rem no device output
20 gosub 30000:rem init arrays
25 :
30 rem (init tokens)
38 :
39 restore 39120
40 read ne:dim em$(ne):for i=1 to ne
50 read em$(i):next
98 :
99 :
100 rem "user interface"
110 pe=1:rem print all errors
120 :
130 df$="test2"
140 print "{reverse on}c{reverse off}ompiler or {reverse on}l{reverse off}inker?"
150 getkey a$
160 if a$="c" then print "{up}{reverse on}compiler{black}{reverse off} or linker?{blue}{down}" : goto 1000
170 if a$="l" then print "{up}{black}compiler or {reverse on}linker{blue}{down}" : goto 50000
180 print "{ct g}";:goto 150
898 :
899 :
900 gosub 10000
910 goto 100
1000 print "filename (.fig is added) ["df$"]"; :f$=df$ :input f$
1010 if right$(f$,4)=".fig" then f$=left$(f$,len(f$)-4)
1015 scratch (f$+".obj")
1020 open 1,8,2,f$+".fig,s,r"
1030 of=2:open of,8,3,f$+".obj,u,w"
1035 tl=0 : rem length of these types is 0
1040 ty$="code" : gosub 26000 : sc$=chr$(64+b) :rem select and name code seg
1045 ty$="math.stack" : gosub 26000 : mb=b     :rem select and name math stack
1046 rem symbol math.space is declared at end
1049 gosub 5000         :rem build function segment
1050 :
1060 cs = -1 :rem current segment
1070 m0 = 4  :rem max number of bytes the math stack needs so far
1099 :
1100 input#1,l$ :print l$ :s1=st :if l$="" then begin :if s1=0 then 1100               :else 1800 :bend
1101 p=1 : l=len(l$)
1102 if p <= l then  if mid$(l$,p,1) <> " " then p=p+1 :goto 1102
1104 w$ = left$(l$,p-1) : if p < l then p$=right$(l$,l-p) :else p$ = ""
1110 gosub 24000 :rem find word
1120 if d%<>0 then begin      :rem word not found
1130 : if w$="decl" then gosub 3000 :goto 1170
1140 :
1160 : print "unknown command"           :rem above ifs didn't find cmd
1170 bend : else begin
1180 : w0% = wl% : print "{space*2}word #";w0%
1185 : if left$(bt$(b(w0%)), 4) = "var-" then f$=l$ : gosub 10000 :else begin
1190 :   if p$ <> "" then gosub 31000 :rem compile params
1200 :   s=0 : m$=dc$(dr(w0%)) : gosub 34000 :rem write data
1205 : bend
1210 bend
1220 if s1=0 then 1100
1799 :
1800 s=0 : m$ = chr$(96)
1810 gosub 34040
1820 :
1830 ty$="math.stack" : gosub 26000
1840 w$="math.space"  : gosub 27000        :rem def sym for stack
1850 l$=chr$(195)+chr$(n(wl%))+chr$(m0)+chr$(0)
1860 gosub 23000
1899 :
1900 print "{down}{reverse on}W compiler is done W"
1950 close of:close 1
1960 end
1998 :
1999 :
3000 rem "decl" a variable
3020 : for p1=4 to l
3030 :   if mid$(l$,p1,1)<>" " then next
3040 : if p1>l then en=1:l=330:ptr=5:            gosub 39000:goto 100
3050 : for p2=p1+1 to l
3060 :   if mid$(l$,p2,1)<>" " then next
3070 : if p2>l then en=13:l=336:ptr=p2:          gosub 39000:goto 100
3080 : w$=mid$(l$,p1+1,p2-p1-1) : ty$=mid$(l$,p2+1,l-p2-1)
3090 : rem determine byte length of type ty$
3100 : t$=left$(ty$,1) : if t$="i" or t$="x" then tl=2 : else tl=5
3110 : ty$="var-"+ty$
3120 : gosub 26000:if en<>0 then 100
3130 : gosub 27000:if en<>0 then 100
3140 : rem   "variable '";vr$;"' created as ";nt$;",";tl;"bytes long."
3150 : l$=chr$(195)+chr$(n(wl%))+chr$(tl)+chr$(0)
3160 : gosub 23000
3170 return
4998 :
4999 :
5000 restore 6000
5010 ty$="func":gosub 26000
5020 read w$:if w$="{arrow left}end" then 5080
5030 gosub 27000:if en<>0 then 39000
5040 dr(wl%)=nd:read dp$(nd)
5050 read v$:if v$<>"{arrow left}" then dc$(nd)=dc$(nd)+chr$(dec(v$)):goto 5050
5060 nd=nd+1:goto 5020
5080 return
6000 data open,"i1 @ a;i1 @ x;i1 @ y"
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
6140 data test,"i2 @ c000;i1 @ ay"
6150 data {arrow left}
6160 :
6170 data {arrow left}end
8999 :
9000 close of
9010 close 1
9090 end
9997 :
9998 :
9999 :
10000 rem compile an expression
10002 rem   input:   f$ is string to compile
10004 rem   output:  math stack contains result
10018 :
10020 ptr=0:part=1:sp=1:en=0:pe=1:ms=0
10030 f$=f$+"{arrow left}"    :rem sentinel terminator
10050 :
10060 rem get next char
10070 ptr=ptr+1
10080 c$=mid$(f$,ptr,1)
10082 if c$=" "  then  10060
10085 if c$="{arrow left}"  then  op$=c$ : gosub 21000 : gs(mb)=2*gl(mb) : return
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
10170 if c$<>"+"and c$<>"*"and c$<>"/"and c$<>"^"and c$<>"="andc$<>";"then 10210    :rem ';' replaces ','
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
10360 :  if pr>=10 then pr=pr-10
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
20050 if c$>="0" and c$<="9" then vr$=vr$+c$:goto 20020
20060 if c$<>"." then 20090
20070 : if pd=1 then 20120
20080 : pd=1:vr$=vr$+c$:goto 20020
20090 if c$<>"e" then 20120
20100 : if ee=1 then 20120
20110 : ee=1:vr$=vr$+c$:goto 20020
20120 ptr=ptr-1
20125 :
20129 rem select bank
20130 ty$="constants" : gosub 26000
20132 rem convert constant to 16-bit int
20138 n=val(vr$) : n$=""
20140 for i=1 to 2  :rem 2 can be replaced with any # bytes to generate
20143 : n=n/256:n$=n$+chr$( (n - int(n)) * 256 + .2 ):n=int(n)
20146 : next
20148 :
20149 rem build internal rep. and generate linker codes
20150 w=nn(b) : nn(b)=nn(b)+1 : vr$=chr$(b)+chr$(w)+chr$(0)+chr$(0)
20155 l$=chr$(195)+chr$(w)+chr$(0)+chr$(0)     :rem define symbol
20160 l$=l$+chr$(2)+n$                         :rem give it a value
20170 gosub 23000
20190 goto 10060
20198 :
20199 :
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
20330 vr$=chr$(b(wl%)) + chr$(n(wl%)) + chr$(0)+chr$(0)
20340 nt$=bt$(b(wl%))
20350 return
20498 :
20499 :
21000 rem push var,op,priority
21005 v2$=vr$:n2$=nt$
21010 op=5    :rem if op$ is a func
21020 if op$="{arrow left}" or op$=";" then op=0:pr=0      :rem force end of expression
21030 if op$="=" then op=1
21040 if op$="+" or op$="-" then op=2
21050 if op$="*" or op$="/" then op=3
21060 if op$="^" then op=4
21065 op=op+pr
21067 if sp<=1 then 21220         :rem empty stack!
21070 if op>ps(sp-1) then pv$=vr$:             goto 21210
21080 tp$=op$:tr=op
21090 :
21100 if v2$="t" then v2$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
21102 gosub 22000:v1$=vr$:n1$=nt$
21105 if v1$="t" then v1$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
21110 if v1$="t+" then ms=ms-1:v1$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
21114 rem define accumulator t
21115 vt$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
21117 ty$="code" : gosub 26000
21120 if op$="+" then 41000
21130 if op$="-" then 42000
21140 if op$="*" then 43000
21150 if op$="/" then 44000
21160 if op$="^" then 45000
21165 if op$="=" then 46000
21167 if op$=";" then 21220       :rem is comma -> next part of 'tuple'
21170 l=21170:en=3:gosub 39000:return
21180 v2$="t"
21190 if sp=1 then 21200
21195 if tr<=ps(sp-1) then 21100
21200 op$=tp$:op=tr
21210 if vs$(sp-1)<>"t" then 21220
21212 vs$(sp-1)="t+"
21213 rem actual pushes not needed; t is actually top of stack
21216 ms=ms+1 :if ms>mm then print "math.stack overflow!!" : return
21218 if 4*ms>m0 then m0=4*ms :rem check size of bank
21220 vs$(sp)=v2$
21225 os$(sp)=op$
21228 if op$=";" then op=-1+pr    :rem stop further ops from getting past here
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
23000 rem write linker data
23001 rem   l$ = data to write
23010 :
23020 print "< linker codes ";
23025 if len(l$) < 1 then  print "{ct o}0 bytes! {143}";
23030 for i=1 to len(l$):printright$(hex$(asc(mid$(l$,i,1))),2)" ";:next
23040 print ">";tab(55)n1$;tab(67)n2$
23050 if of<>0 then print#of,l$;
23060 if po=1 then print#4,l$;
23070 return
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
26000 rem open a bank (segment) and select it
26005 en=0
26010 if nb=0 then b=0:goto 26050
26020 for b=0 to nb-1
26025 rem hunt for bank; if not found, select it only if not currently sel'd
26027 rem   else create new bank
26030 : if ty$=bt$(b) then begin : print "<<select bank:";b;">>":l$=chr$(64+b):         if cs <> b then cs=b:goto 26070 : else goto 26080 : bend
26040 : next
26042 rem create new bank, if space permits
26045 if b>bs then en=15:l=26045:              gosub 39000:goto 27070
26050 nb=nb+1 : bt$(b)=ty$ : tl(b)=tl
26055 print "<<new bank:";b;ty$;">>"
26060 l$=chr$(64+b)+chr$(196)+ty$+chr$(0)
26070 gosub 23000
26080 return
26999 :
27000 rem insertion sort one word (w$) into bank b
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
31025 :
31030 dp$=dp$(dr(wl%)):if dp$ = "" then return
31032 print " (compile actual params: template='";dp$;"', param='";p$;"')"
31034 f$=p$:gosub 10000:if en <> 0 then return
31040 it=0 : m$="" : s2=0        :rem s2=sp for fetching params
31050 print " (what var type?) ";
31060 : s$="" : s2=s2+1
31070 : it=it+1:v$=mid$(dp$, it, 1)
31080 : it=it+1:c$=mid$(dp$, it, 1)
31090 : if c$>="0" and c$<="9" then s$=s$+c$:goto 31080
31100 : print" var type: ";v$;" - ";
31110 :
31120 : if v$="i" then gosub 32000:goto 31160
31130 : if v$="s" then gosub 33000:goto 31160
31140 : print "unknown: "v$;c$: goto 9000
31150 :
31160 if it>1 and s2<sp-1 then 31050
31170 :
31172 if it=0 and s2<sp-1 then en=18:l=31172:gosub 39000
31174 if it>0 and s2=sp-1 then en=19:l=31174:gosub 39000
31180 ty$="code" : gosub 26000 : l$=m$ :gosub 23000 :rem select cs, ins data
31190 return
31200 :
32000 if s$="" then print"missing int size": goto 32500
32010 s=val(s$):if s<1 or s>4 then print"unsupported int size":goto 32500
32020 :
32140 it=it+1:if mid$(dp$, it, 1) <> "@" then print"unknown template command":goto 32500
32150 it=it+1
32160 :
32170 print " (fetch storage location) ";
32180 l$="":lt=0 :rem lt=loc type: 0=dec ad, 1=hex ad, 2=reg
32190 it=it+1:c$=mid$(dp$, it, 1)
32200 :  if instr("0123456789", c$) > 0  then   l$=l$+c$:goto 32190
32210 :  if c$="$" then                         lt=1:goto 32190
32220 :  if lt=1 and instr("abcdef", c$) > 0  then l$=l$+c$:goto 32190
32230 :  if instr("axy", c$) > 0  then lt=2    :l$=l$+c$:goto 32190
32240 if lt=2 then 32380: rem handle register storage
32250 if lt=1 then l=dec(right$(l$,len(l$)-1))
32260 if lt=0 then l=val(l$)
32270 :
32280 print " (generate code to store the bytes) ";
32285 v$=vs$(s2)
32287 if v$="t" then v$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
32290 for i=0 to s-1
32300 :  m$ = m$ + "{ct a}"+chr$(dec("ad"))+chr$(226)+left$(v1$,2)+chr$(asc(mid$(v1$,3,1))+i)+mid$(v1$,4,1)
32310 :  lh=int(l/256) :ll=l-lh*256
32320 :  m$ = m$ + "{ct c}"+chr$(141)+chr$(ll)+chr$(lh)
32330 :  l = l+1
32340 :  next
32350 :
32360 goto 32500
32370 :
32380 if len(l$) <> s then print "number of regs doesn't match specified int size":goto 32500
32390 :
32400 rem set up ld? instructions
32410 ld(1)=173:ld(2)=174:ld(3)=172
32420 :
32430 print " (load reg(s)) ";
32435 v$=vs$(s2)
32437 if v$="t" then v$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
32438 rem this next line shouldn't be needed, but what's "t+"?
32439 if v$="t+" then stop : rem ms=ms-1:v$=chr$(mb)+chr$(0)+chr$(4*ms)+chr$(0)
32440 for i=1 to len(l$)
32450 : r$ = "{ct a}"+chr$(ld(instr("axy",mid$(l$,i,1))))                                    + chr$(226) + left$(v$,2) + chr$(asc(mid$(v$,3,1))+i-1) + mid$(v$,4,1)
32455 : m$=m$+r$:printlen(r$);
32460 : next
32470 :
32480 goto 32500
32490 :
32500 print " (advance indices to next ';')"
32510 it=instr(dp$, ";", it)
32530 :
32550 return
32560 :
33000 return :rem strings not implemented yet
33998 :
33999 :
34000 rem generate 'write data' linker command
34010 rem   s  = segment to insert data into
34020 rem   m$ = data to write
34030 :
34031 print "('write data':";len(m$);" bytes)"
34035 if m$ <> "" then begin    :rem write only if more than 0 bytes
34040 :  rem if s<64, use 'quick select':
34050 :  if s <> cs then cs = s :print#of,chr$(64+s);
34060 :  rem if len(m$) < 64, use quick write:
34070 :  print#of,chr$(len(m$));m$; :rem insert byte count & data
34075 bend
34080 return
39000 rem report errors
39005 if pe=0 then return
39010 if en<1 or en>ne then print"bad error # ("en") in"l:return
39020 if ptr>0 then print " {left}"tab(ptr+1);"^ ("c$")";
39030 print
39040 print "error in line"l"{left}:":printem$(en)
39050 return
39060 :
39070 :
39100 :
39110 rem errors-for ln 40
39120 data 19,missing var,early end of line,syntax,too many '='
39130 data illegal self operator,equation left of '=',illegal op this side of '='
39140 data illegal variable name,divide by 0,stack overflow,undefined variable
39150 data too many close parenthesis,unknown var type,var already exists
39160 data too many banks,too many tokens in this bank,assignment to a constant
39170 data too many parameters,too few parameters
39998 :
39999 :
41000 rem generate 16 bit addition code
41010 l$=    "{ct b}"+chr$(24)+chr$(dec("ad"))+chr$(226)+v1$
41020 l$=l$ +"{ct a}"         +chr$(dec("6d"))+chr$(226)+v2$
41030 l$=l$ +"{ct a}"         +chr$(dec("8d"))+chr$(226)+vt$
41040 l$=l$ +"{ct a}"         +chr$(dec("ad"))+chr$(226)+left$(v1$,2)+chr$(asc(mid$(v1$,3,1))+1)+mid$(v1$,4,1)
41050 l$=l$ +"{ct a}"         +chr$(dec("6d"))+chr$(226)+left$(v2$,2)+chr$(asc(mid$(v2$,3,1))+1)+mid$(v2$,4,1)
41060 l$=l$ +"{ct a}"         +chr$(dec("8d"))+chr$(226)+left$(vt$,2)+chr$(asc(mid$(vt$,3,1))+1)+mid$(vt$,4,1)
41100 gosub 23000
41900 goto 21180
41998 :
41999 :
42000 rem generate 16 bit subtraction code
42010 l$=    "{ct b}"+chr$(56)+chr$(dec("ad"))+chr$(226)+v1$
42020 l$=l$ +"{ct a}"         +chr$(dec("ed"))+chr$(226)+v2$
42030 l$=l$ +"{ct a}"         +chr$(dec("8d"))+chr$(226)+vt$
42040 l$=l$ +"{ct a}"         +chr$(dec("ad"))+chr$(226)+left$(v1$,2)+chr$(asc(mid$(v1$,3,1))+1)+mid$(v1$,4,1)
42050 l$=l$ +"{ct a}"         +chr$(dec("ed"))+chr$(226)+left$(v2$,2)+chr$(asc(mid$(v2$,3,1))+1)+mid$(v2$,4,1)
42060 l$=l$ +"{ct a}"         +chr$(dec("8d"))+chr$(226)+left$(vt$,2)+chr$(asc(mid$(vt$,3,1))+1)+mid$(vt$,4,1)
42100 gosub 23000
42900 goto 21180
42998 :
42999 :
43000 rem vt$ = v1$ * v2$
43010 l$="{ct a}"+chr$(dec("ea")) :gosub 23000
43900 goto 21180
43998 :
43999 :
44000 rem vt$ = v1$ / v2$
44010 l$="{ct a}"+chr$(dec("ea")) :gosub 23000
44900 goto 21180
44998 :
44999 :
45000 rem vt$ = v1$ ^ v2$
45010 l$="{ct a}"+chr$(dec("ea")) :gosub 23000
45190 return
45900 goto 21180
45998 :
45999 :
46000 rem generate 16 bit assignment code:  v2$ = v1$
46010 if left$(n1$,4)<>"var-" then en=17:l= 46000:goto 39000 :rem asn to const
46020 l$=    "{ct a}"+chr$(dec("ad"))+chr$(226)+v2$
46030 l$=l$ +"{ct a}"+chr$(dec("8d"))+chr$(226)+v1$
46040 l$=l$ +"{ct a}"+chr$(dec("ad"))+chr$(226)+left$(v2$,2)+chr$(asc(mid$(v2$,3,1))+1)+mid$(v2$,4,1)
46050 l$=l$ +"{ct a}"+chr$(dec("8d"))+chr$(226)+left$(v1$,2)+chr$(asc(mid$(v1$,3,1))+1)+mid$(v1$,4,1)
46100 gosub 23000
46900 goto 21180
46998 :
46999 :
50000 rem fig linker
50001 :
50004 open 5,8,15,"i":close 5:if ds<>0 then printds$:goto 59100
50010 f$=df$
50012 print ".obj file to link [";f$;"]";:input f$
50014 if right$(f$,4) <> ".obj" then i$=f$+".obj": else i$=f$:                       f$=left$(i$,len(i$)-4)
50018 open 2,8,2,i$:close 2:if ds<>0 then print ds$:goto 50005
50020 o$=f$+".ml"
50021 print "executable name [";o$;"]";:input o$
50023 open 2,8,2,o$:close 2:if ds=0 then print "-scratching file":scratch (o$)
50025 print "link: in ";i$;" and out ";o$ : print
50030 :
50040 rem --- general inits ---
50050 z$=chr$(0) : mb = 20 : rem max banks
50060 dim sl(mb,20) : rem symbol location
50070 dim gt$(mb),ga(mb),gs(mb) : rem segment titles, addresses, size (bytes)
50080 dim gl(mb) : rem segment length (words)
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
51030 if ps = 1  then gs(sg) = gs(sg) + b
51040 if ps = 2  and  sg = zi - 1  then 51500
51050 : rem skip the data:
51060 if b>0 then   for i=1 to b : get#2,c$ : next
51070 return
51498 :
51499 rem tranfer data to executable (pass 2)
51500 : for i=1 to b  :rem transfer the data
51510 if b>0 then   for i=1 to b : get#2,c$ : print#3,left$(c$+z$,1); : next
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
54200 rem define symbol in segment sg
54210 gosub 60000 : s=b    :rem symbol #
54220 gosub 60100          :rem # bytes
54225 print "create symbol"; s; " in segment"; sg;
54228 if ps=1 then begin
54230 : if gl(sg)<=s then gl(sg)=s+1
54232 : sl(sg, s) = gs(sg) : gs(sg) = gs(sg) + b
54234 : bend : else begin                       :rem reserve space for var
54236 :   if sg = zi-1 and b > 0 then  for i=1 to b:print#3,z$;:next
54238 :   bend
54240 print ", offset"; sl(sg,s); ",";b;" byte";:if b<>1 then print"s":else print
54250 return
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
55052 :
55055 if ps=1  then  gs(sg)=gs(sg)+l    :rem update seg size during pass 1
55057 if ps=2 and sg=zi-1 then begin    :rem do the insertion if at right seg
55060 : if s>=gl(g) then print "{ct o}{space*3}*** symbol is not defined ***":return
55062 : v=sl(g,s) + o                   :rem  (get val to ins)
55065 : if d=1 then begin               :rem  high byte first
55070 :   l=l-1 : d=256^int(log(v)/log(2)/8+.99)    :rem # bytes
55080 :   for i=l to 0 step -1                  :rem high byte first
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
55180 : bend   :rem if ps=2
55190 return
55998 :
55999 :
59000 print "{down}-one run of pass";ps;"done-{down}"
59010 close 2                 :rem close input file
59015 if ps=2 then ad = ad+gs(zi-1)
59020 next zi                 :rem repeat this pass
59025 print "{reverse on}W{space*2}pass";ps;"{left} done{space*2}W"
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
62000 open 1,8,2,"test2.fig"
62010 input#1,a$:printa$ :if st=0 then 62010
62020 close 1
62030 end
62200 open 1,8,2,"test2.obj":z$=chr$(0)
62210 get#1,a$:printright$(hex$(asc(a$+z$)),2);:if st<>0 then 62230
62220 for i=1 to 15:get#1,a$:print", "right$(hex$(asc(a$+z$)),2);:if st<>0 then 62230 :else next:print:goto 62210
62230 print:close 1
62240 end

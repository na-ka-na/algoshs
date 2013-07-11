$algo$_MT_$idSuffix$ (f, w, b) [MT]

vars        : n  __ bank_count(1)
              ws __ words_array(n,w)

formulae    : p  = MT_BITS(n + $numSpares$ - 1)
              w' = ws.max()

macros:
type1(BASE)       : n * MM_$bank$(f, ws, b)
type2(BASE,SPARE) : $numSpares$ * MM_$bank$(f, w', b)
type3(MT)         : 1 * MM_$state$(f, w', p)

mprops      : numBanks = n

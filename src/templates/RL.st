$algo$_$idSuffix$ (f, w, b) [RL]

vars        : n  __ bank_count(1)
              ws __ words_array(n,w)

formulae    : p  = TAG_BITS(n)
              w' = ws.max()

macros:
type1(BASE) : n * MM_$bank$(f, ws, b)
type2       : $numTags$ * MM_$tag$(f, w', b)
type3       : $numTags$ * MM_$tag$(f, w', p)

mprops      : numBanks = n

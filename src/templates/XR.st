$algo$_XR_$idSuffix$ (f, w, b) [XR]

vars        : n  __ bank_count(1)
              ws __ words_array(n,w)

formulae    : w' = ws.max()

macros:
type1(BASE)       : n * MM_$bank$(f, ws, b)
type2(BASE)       : 1 * MM_$bank$(f, w', b)

mprops      : numBanks = n

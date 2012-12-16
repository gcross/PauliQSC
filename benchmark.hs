-- Language extensions {{{

{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

-- Imports {{{
import Criterion.Main

import Data.Quantum.Small.Operator
import Data.Quantum.Small.Operator.SubsystemCode
import Data.Quantum.Small.Operator.ReducedEschelonForm
-- }}} Imports

main = defaultMain
    -- Benchmarks {{{
    [bgroup "construction of reduced eschelon form" -- {{{
        [bench "4 ops" $ -- {{{
            whnf constructReducedEschelonForm
            [read "IIZZZIYI"
            ,read "ZIXXXZZZ"
            ,read "YYZIIIXX"
            ,read "YIYIZIIZ"
            ]
         -- }}}
        ,bench "8 ops" $ -- {{{
            whnf constructReducedEschelonForm
            [read "IIXIXIXY"
            ,read "XIYZYXZZ"
            ,read "ZZYZIZZX"
            ,read "IXIIYIZY"
            ,read "ZZZYZZYX"
            ,read "YYZIXZIX"
            ,read "ZXYYXIXI"
            ,read "IIIIIYIX"
            ,read "YYIZIXXZ"
            ,read "IIYZZYZY"
            ,read "ZYXYXZXI"
            ,read "IZIIZZIY"
            ]
         -- }}}
        ,bench "16 ops" $ -- {{{
            whnf constructReducedEschelonForm
            [read "YXIZYZII"
            ,read "XYZZZXZZ"
            ,read "XIYXIXZZ"
            ,read "ZZIZYXYZ"
            ,read "ZIIZZZXZ"
            ,read "ZIIXIZIY"
            ,read "ZXYIYYZZ"
            ,read "XXZXYXXI"
            ,read "IYYZIIIX"
            ,read "XXIYIXZY"
            ,read "IYYXXXXI"
            ,read "XZXZXXYX"
            ,read "XYXXXIYX"
            ,read "ZIYXIZYI"
            ,read "IZIIIXZI"
            ,read "XYXYIZZZ"
            ]
         -- }}}
        ,bench "32 ops" $ -- {{{
            whnf constructReducedEschelonForm
            [read "ZYIYYXZY"
            ,read "IIYYZZXI"
            ,read "IYYIIYYI"
            ,read "XYIXXYIX"
            ,read "ZIZZYZXI"
            ,read "IIZXXIXI"
            ,read "IYXYIIZZ"
            ,read "YZIZXYYZ"
            ,read "IIYIYYZI"
            ,read "IXIYYZXI"
            ,read "YIXXYIXZ"
            ,read "XIZZIXZZ"
            ,read "YIYXZXXI"
            ,read "IXZYIYII"
            ,read "YYIZYZYZ"
            ,read "ZZZXIIXZ"
            ,read "IIZZIZII"
            ,read "XZYYYYZY"
            ,read "IXXZIIZY"
            ,read "IIIZYIXZ"
            ,read "XYXYXYYI"
            ,read "IYXYXZYY"
            ,read "YXXYXXXZ"
            ,read "ZIYZYXII"
            ,read "YZZYZZYX"
            ,read "IZZZYZZX"
            ,read "IYZYIZYZ"
            ,read "IYYZYXZI"
            ,read "YYIXYZIZ"
            ,read "YYXXZZIX"
            ,read "YYYIZZZI"
            ,read "YZYIIIXI"
            ]
         -- }}}
        ]
     -- }}}
    ,bgroup "construction of subsystem code" -- {{{
        [bench "4 ops" $ -- {{{
            whnf (constructSubsystemCodeFromMeasurements 8)
            [read "IIZZZIYI"
            ,read "ZIXXXZZZ"
            ,read "YYZIIIXX"
            ,read "YIYIZIIZ"
            ]
         -- }}}
        ,bench "8 ops" $ -- {{{
            whnf (constructSubsystemCodeFromMeasurements 8)
            [read "YIIIIXZI"
            ,read "IXIIYYYZ"
            ,read "ZYXYZIZX"
            ,read "IYZXXYIZ"
            ,read "YZZYXXYX"
            ,read "ZZIXIZIZ"
            ,read "YXYIXYXI"
            ,read "YIXIXYZY"
            ]
         -- }}}
        ,bench "12 ops" $ -- {{{
            whnf (constructSubsystemCodeFromMeasurements 8)
            [read "IIXIXIXY"
            ,read "XIYZYXZZ"
            ,read "ZZYZIZZX"
            ,read "IXIIYIZY"
            ,read "ZZZYZZYX"
            ,read "YYZIXZIX"
            ,read "ZXYYXIXI"
            ,read "IIIIIYIX"
            ,read "YYIZIXXZ"
            ,read "IIYZZYZY"
            ,read "ZYXYXZXI"
            ,read "IZIIZZIY"
            ]
         -- }}}
        ,bench "16 ops" $ -- {{{
            whnf (constructSubsystemCodeFromMeasurements 8)
            [read "YXIZYZII"
            ,read "XYZZZXZZ"
            ,read "XIYXIXZZ"
            ,read "ZZIZYXYZ"
            ,read "ZIIZZZXZ"
            ,read "ZIIXIZIY"
            ,read "ZXYIYYZZ"
            ,read "XXZXYXXI"
            ,read "IYYZIIIX"
            ,read "XXIYIXZY"
            ,read "IYYXXXXI"
            ,read "XZXZXXYX"
            ,read "XYXXXIYX"
            ,read "ZIYXIZYI"
            ,read "IZIIIXZI"
            ,read "XYXYIZZZ"
            ]
         -- }}}
        ,bench "32 ops" $ -- {{{
            whnf (constructSubsystemCodeFromMeasurements 8)
            [read "ZYIYYXZY"
            ,read "IIYYZZXI"
            ,read "IYYIIYYI"
            ,read "XYIXXYIX"
            ,read "ZIZZYZXI"
            ,read "IIZXXIXI"
            ,read "IYXYIIZZ"
            ,read "YZIZXYYZ"
            ,read "IIYIYYZI"
            ,read "IXIYYZXI"
            ,read "YIXXYIXZ"
            ,read "XIZZIXZZ"
            ,read "YIYXZXXI"
            ,read "IXZYIYII"
            ,read "YYIZYZYZ"
            ,read "ZZZXIIXZ"
            ,read "IIZZIZII"
            ,read "XZYYYYZY"
            ,read "IXXZIIZY"
            ,read "IIIZYIXZ"
            ,read "XYXYXYYI"
            ,read "IYXYXZYY"
            ,read "YXXYXXXZ"
            ,read "ZIYZYXII"
            ,read "YZZYZZYX"
            ,read "IZZZYZZX"
            ,read "IYZYIZYZ"
            ,read "IYYZYXZI"
            ,read "YYIXYZIZ"
            ,read "YYXXZZIX"
            ,read "YYYIZZZI"
            ,read "YZYIIIXI"
            ]
         -- }}}
        ]
    -- }}}
    ]

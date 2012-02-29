-- Language extensions {{{

{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

-- Imports {{{
import Criterion.Main

import Data.Word

import Data.Quantum.Operator
import Data.Quantum.Operator.SubsystemCode
import Data.Quantum.Operator.ReducedEschelonForm
-- }}} Imports

main = defaultMain
    -- Benchmarks {{{
    [bgroup "construction of reduced eschelon form" -- {{{
        [bgroup "8 bits" -- {{{
            [bench "4 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "IIZZZIYI" :: Operator Word8
                ,read "ZIXXXZZZ" :: Operator Word8
                ,read "YYZIIIXX" :: Operator Word8
                ,read "YIYIZIIZ" :: Operator Word8
                ]
             -- }}}
            ,bench "8 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "YIIIIXZI" :: Operator Word8
                ,read "IXIIYYYZ" :: Operator Word8
                ,read "ZYXYZIZX" :: Operator Word8
                ,read "IYZXXYIZ" :: Operator Word8
                ,read "YZZYXXYX" :: Operator Word8
                ,read "ZZIXIZIZ" :: Operator Word8
                ,read "YXYIXYXI" :: Operator Word8
                ,read "YIXIXYZY" :: Operator Word8
                ]
             -- }}}
            ,bench "12 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "IIXIXIXY" :: Operator Word8
                ,read "XIYZYXZZ" :: Operator Word8
                ,read "ZZYZIZZX" :: Operator Word8
                ,read "IXIIYIZY" :: Operator Word8
                ,read "ZZZYZZYX" :: Operator Word8
                ,read "YYZIXZIX" :: Operator Word8
                ,read "ZXYYXIXI" :: Operator Word8
                ,read "IIIIIYIX" :: Operator Word8
                ,read "YYIZIXXZ" :: Operator Word8
                ,read "IIYZZYZY" :: Operator Word8
                ,read "ZYXYXZXI" :: Operator Word8
                ,read "IZIIZZIY" :: Operator Word8
                ]
             -- }}}
            ,bench "16 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "YXIZYZII" :: Operator Word8
                ,read "XYZZZXZZ" :: Operator Word8
                ,read "XIYXIXZZ" :: Operator Word8
                ,read "ZZIZYXYZ" :: Operator Word8
                ,read "ZIIZZZXZ" :: Operator Word8
                ,read "ZIIXIZIY" :: Operator Word8
                ,read "ZXYIYYZZ" :: Operator Word8
                ,read "XXZXYXXI" :: Operator Word8
                ,read "IYYZIIIX" :: Operator Word8
                ,read "XXIYIXZY" :: Operator Word8
                ,read "IYYXXXXI" :: Operator Word8
                ,read "XZXZXXYX" :: Operator Word8
                ,read "XYXXXIYX" :: Operator Word8
                ,read "ZIYXIZYI" :: Operator Word8
                ,read "IZIIIXZI" :: Operator Word8
                ,read "XYXYIZZZ" :: Operator Word8
                ]
             -- }}}
            ,bench "32 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "ZYIYYXZY" :: Operator Word8
                ,read "IIYYZZXI" :: Operator Word8
                ,read "IYYIIYYI" :: Operator Word8
                ,read "XYIXXYIX" :: Operator Word8
                ,read "ZIZZYZXI" :: Operator Word8
                ,read "IIZXXIXI" :: Operator Word8
                ,read "IYXYIIZZ" :: Operator Word8
                ,read "YZIZXYYZ" :: Operator Word8
                ,read "IIYIYYZI" :: Operator Word8
                ,read "IXIYYZXI" :: Operator Word8
                ,read "YIXXYIXZ" :: Operator Word8
                ,read "XIZZIXZZ" :: Operator Word8
                ,read "YIYXZXXI" :: Operator Word8
                ,read "IXZYIYII" :: Operator Word8
                ,read "YYIZYZYZ" :: Operator Word8
                ,read "ZZZXIIXZ" :: Operator Word8
                ,read "IIZZIZII" :: Operator Word8
                ,read "XZYYYYZY" :: Operator Word8
                ,read "IXXZIIZY" :: Operator Word8
                ,read "IIIZYIXZ" :: Operator Word8
                ,read "XYXYXYYI" :: Operator Word8
                ,read "IYXYXZYY" :: Operator Word8
                ,read "YXXYXXXZ" :: Operator Word8
                ,read "ZIYZYXII" :: Operator Word8
                ,read "YZZYZZYX" :: Operator Word8
                ,read "IZZZYZZX" :: Operator Word8
                ,read "IYZYIZYZ" :: Operator Word8
                ,read "IYYZYXZI" :: Operator Word8
                ,read "YYIXYZIZ" :: Operator Word8
                ,read "YYXXZZIX" :: Operator Word8
                ,read "YYYIZZZI" :: Operator Word8
                ,read "YZYIIIXI" :: Operator Word8
                ]
             -- }}}
            ]
         -- 8 bits }}}
        ,bgroup "16 bits" -- {{{
            [bench "4 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "IXYYYIZZYXYXIZXX" :: Operator Word16
                ,read "XIXYYXZZXZIZIIXY" :: Operator Word16
                ,read "IZZXZIXIXYZXYYIX" :: Operator Word16
                ,read "XZYXIIYXYZXXYXZY" :: Operator Word16
                ]
             -- }}}
            ,bench "8 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "XXZXYZIXZZYIIYYX" :: Operator Word16
                ,read "YIYIIZXIIYXYZYIZ" :: Operator Word16
                ,read "ZYIXYYYIIYXZZIIY" :: Operator Word16
                ,read "XZZZYXYYXZYZXIIX" :: Operator Word16
                ,read "ZYIYZXZIIIIXXZXX" :: Operator Word16
                ,read "IXYZXZZXIYYZXIIX" :: Operator Word16
                ,read "XYIXIIYYZZYXIIYZ" :: Operator Word16
                ,read "IZIXIIZYYXIZIZXY" :: Operator Word16
                ]
             -- }}}
            ,bench "16 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "YXIXYYZYZYIXXZYX" :: Operator Word16
                ,read "XYZYZYZXIYXYXZZI" :: Operator Word16
                ,read "IIXZIZZIZIZIYYXX" :: Operator Word16
                ,read "ZXZIZZYXXYYZIXZX" :: Operator Word16
                ,read "XIYXIIYYXYYZZYXY" :: Operator Word16
                ,read "XIZYXYYXIZIZIYZY" :: Operator Word16
                ,read "ZYXIIIIXXYZXZZIY" :: Operator Word16
                ,read "ZZYZYYZIZYXXZIZY" :: Operator Word16
                ,read "YXXZYIXZXXZIYIZY" :: Operator Word16
                ,read "YZXIYIXIIYIYYZXI" :: Operator Word16
                ,read "IZIIZIYIXXIIIXIY" :: Operator Word16
                ,read "IXYXIXYZYXYYIIYZ" :: Operator Word16
                ,read "YYIIYXYYXZXIXIIY" :: Operator Word16
                ,read "IZYXIXZYZYXYXYZI" :: Operator Word16
                ,read "YYZXIYXXXIZZIIYZ" :: Operator Word16
                ,read "ZYXIZYXIZZYXIIZY" :: Operator Word16
                ]
             -- }}}
            ,bench "32 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "XIXIYZZZYXIYZIYZ" :: Operator Word16
                ,read "IZYIZYYIIIXZIXXZ" :: Operator Word16
                ,read "XZIYXIYXIYZXZXZY" :: Operator Word16
                ,read "YYIZIIIIXXZYZZYY" :: Operator Word16
                ,read "IZYYYIIXYYXXZYZX" :: Operator Word16
                ,read "ZIIZYZZYIYXIXIIY" :: Operator Word16
                ,read "XXXXIXZXIZXIIYYI" :: Operator Word16
                ,read "IXYYZIIZZIXYYZZX" :: Operator Word16
                ,read "YXXZZZYIZIXIZZIX" :: Operator Word16
                ,read "YXYIYZYXYIIXZZXZ" :: Operator Word16
                ,read "YZIIIXZXYYXIXIZI" :: Operator Word16
                ,read "YIZIXYZXYYZIIZXI" :: Operator Word16
                ,read "IZYZXXZIYIIYYYXX" :: Operator Word16
                ,read "IIXXIYIYIXXIXIYX" :: Operator Word16
                ,read "IYIZYYXZYZYIIIZI" :: Operator Word16
                ,read "YXYZYXZZXIYYIZXI" :: Operator Word16
                ,read "XZIXIYYXYZIYYXYZ" :: Operator Word16
                ,read "YXYZXXIZZIYXIZIZ" :: Operator Word16
                ,read "ZIXYZYYXZYZZZZZI" :: Operator Word16
                ,read "ZZZYIYXXIXZXZYZI" :: Operator Word16
                ,read "IYIZYZXYYYIYIZYX" :: Operator Word16
                ,read "YYYZXYIXXYZIXYXY" :: Operator Word16
                ,read "XXIYZYXZXIIIXYZX" :: Operator Word16
                ,read "IXIZZYZYYZXIIYXY" :: Operator Word16
                ,read "XYZZZIZXYXIXXXII" :: Operator Word16
                ,read "IYXYIZZZZZIYZIZI" :: Operator Word16
                ,read "YXYYXIZZZYYIXIXY" :: Operator Word16
                ,read "ZYXZXXIYYYXIYZIZ" :: Operator Word16
                ,read "IXXZXYXZYZZXYZXI" :: Operator Word16
                ,read "IYIXIXYIYYYZIXXI" :: Operator Word16
                ,read "XZXIZIZZXXYZYIZX" :: Operator Word16
                ,read "IYZIZIZZXZXZYYXZ" :: Operator Word16
                ]
             -- }}}
            ,bench "64 ops" $ -- {{{
                whnf constructReducedEschelonForm
                [read "XIYIXIYXYZZXXXXZ" :: Operator Word16
                ,read "XIZIXXYZXZYZZIZZ" :: Operator Word16
                ,read "IYXIIXZZZYXZXYYX" :: Operator Word16
                ,read "IZIYIYXIZIXIZIXX" :: Operator Word16
                ,read "IXZYYXYYYYXYYZZX" :: Operator Word16
                ,read "IIZIXZYIXXYZYZXX" :: Operator Word16
                ,read "ZYXXYIXIZXXIXIYZ" :: Operator Word16
                ,read "YXXXXXXZXYYZIXXY" :: Operator Word16
                ,read "ZIYIIZYXXZYIYZYY" :: Operator Word16
                ,read "XIZYXYYZIZZZYIYZ" :: Operator Word16
                ,read "IXZZIZXXIYXYXIIX" :: Operator Word16
                ,read "ZIYZZZZXIZXZXXZI" :: Operator Word16
                ,read "IIYZXXZXXXZXXIYY" :: Operator Word16
                ,read "YIYXXZYYIIZZIZII" :: Operator Word16
                ,read "XYXIXXYIIIYYYXXZ" :: Operator Word16
                ,read "XXYYZXYYIZXIZZYY" :: Operator Word16
                ,read "IIXZZIYXIYZIIZZI" :: Operator Word16
                ,read "XZIZIIIYXIZXXZYI" :: Operator Word16
                ,read "IXYIIYXZZIIXZIZX" :: Operator Word16
                ,read "XIXXIIIXXYXIYIYX" :: Operator Word16
                ,read "ZXIZXXYXYYIXIIYZ" :: Operator Word16
                ,read "ZZZXZYZXYIYZIXZZ" :: Operator Word16
                ,read "ZIYIIZIYXZYIIZYI" :: Operator Word16
                ,read "IXXYXIYZXXXXIZIX" :: Operator Word16
                ,read "ZYXIXIIIZXZZZZYI" :: Operator Word16
                ,read "IXYIXIIYYYZZIYZX" :: Operator Word16
                ,read "IYYIYIIYZXIYXIYZ" :: Operator Word16
                ,read "IYYIZZIXXIXXYXIX" :: Operator Word16
                ,read "XIXZYYYXXZZIZZIZ" :: Operator Word16
                ,read "IYXIYYZIZZZZXIXI" :: Operator Word16
                ,read "IZZZYXXXZIYIXXZX" :: Operator Word16
                ,read "IIYZIIYXIXXXIZYZ" :: Operator Word16
                ,read "YZYIYZZIYZIIIXXX" :: Operator Word16
                ,read "IXIXZZZYIZXYZZXX" :: Operator Word16
                ,read "ZXYXYXXZXIZYYXIY" :: Operator Word16
                ,read "ZXIZXZZXXZZZZYIY" :: Operator Word16
                ,read "YZXZZXIXZYYIYZIY" :: Operator Word16
                ,read "YZZIXIZXIYYXIYXI" :: Operator Word16
                ,read "ZZZIXIZIZZZYXXZZ" :: Operator Word16
                ,read "ZIYYIXZIZYZZZIYY" :: Operator Word16
                ,read "XZIXZXXIZZIYIXYY" :: Operator Word16
                ,read "YZZYYXYYXYXZZYZZ" :: Operator Word16
                ,read "ZZYIYZYZXIXZIIYY" :: Operator Word16
                ,read "IYYXZIZXXYYIZXXX" :: Operator Word16
                ,read "IXYXXYXIIXIIZYXX" :: Operator Word16
                ,read "YYIZIZZIZZZIZYYY" :: Operator Word16
                ,read "XXYXYXXYYIXXIXXX" :: Operator Word16
                ,read "XYIZXXYIZXYZIIYX" :: Operator Word16
                ,read "IXYIXZZYZYIIZXIX" :: Operator Word16
                ,read "YXIXXZIXZYYIZIZX" :: Operator Word16
                ,read "ZXXIYXXZIIXYIYXZ" :: Operator Word16
                ,read "IZXIXYXXZIZIZIZZ" :: Operator Word16
                ,read "IXXIIIZZYIXYYXZX" :: Operator Word16
                ,read "XZYZYXYYIIXIZZXY" :: Operator Word16
                ,read "IXXYYZZYIIYZXIYI" :: Operator Word16
                ,read "YYZIZXYYZZXIXYXI" :: Operator Word16
                ,read "ZIIYZXYYXYIYYXZI" :: Operator Word16
                ,read "XXZZXXIZYXYIZIYY" :: Operator Word16
                ,read "ZYXYZZIYXIYZIYYX" :: Operator Word16
                ,read "ZXYIXZXXZXZXIYIX" :: Operator Word16
                ,read "IIXXYZXXYXYZZXIY" :: Operator Word16
                ,read "YIZZIZZYXIZYZXZX" :: Operator Word16
                ,read "ZIXYZXIYYZIIIYIX" :: Operator Word16
                ,read "IIIYIZXYZXYZIZZY" :: Operator Word16
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}} construction of reduced eschelon form
    ,bgroup "construction of subsystem code" -- {{{
        [bgroup "8 bits" -- {{{
            [bench "4 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 8)
                [read "IIZZZIYI" :: Operator Word8
                ,read "ZIXXXZZZ" :: Operator Word8
                ,read "YYZIIIXX" :: Operator Word8
                ,read "YIYIZIIZ" :: Operator Word8
                ]
             -- }}}
            ,bench "8 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 8)
                [read "YIIIIXZI" :: Operator Word8
                ,read "IXIIYYYZ" :: Operator Word8
                ,read "ZYXYZIZX" :: Operator Word8
                ,read "IYZXXYIZ" :: Operator Word8
                ,read "YZZYXXYX" :: Operator Word8
                ,read "ZZIXIZIZ" :: Operator Word8
                ,read "YXYIXYXI" :: Operator Word8
                ,read "YIXIXYZY" :: Operator Word8
                ]
             -- }}}
            ,bench "12 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 8)
                [read "IIXIXIXY" :: Operator Word8
                ,read "XIYZYXZZ" :: Operator Word8
                ,read "ZZYZIZZX" :: Operator Word8
                ,read "IXIIYIZY" :: Operator Word8
                ,read "ZZZYZZYX" :: Operator Word8
                ,read "YYZIXZIX" :: Operator Word8
                ,read "ZXYYXIXI" :: Operator Word8
                ,read "IIIIIYIX" :: Operator Word8
                ,read "YYIZIXXZ" :: Operator Word8
                ,read "IIYZZYZY" :: Operator Word8
                ,read "ZYXYXZXI" :: Operator Word8
                ,read "IZIIZZIY" :: Operator Word8
                ]
             -- }}}
            ,bench "16 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 8)
                [read "YXIZYZII" :: Operator Word8
                ,read "XYZZZXZZ" :: Operator Word8
                ,read "XIYXIXZZ" :: Operator Word8
                ,read "ZZIZYXYZ" :: Operator Word8
                ,read "ZIIZZZXZ" :: Operator Word8
                ,read "ZIIXIZIY" :: Operator Word8
                ,read "ZXYIYYZZ" :: Operator Word8
                ,read "XXZXYXXI" :: Operator Word8
                ,read "IYYZIIIX" :: Operator Word8
                ,read "XXIYIXZY" :: Operator Word8
                ,read "IYYXXXXI" :: Operator Word8
                ,read "XZXZXXYX" :: Operator Word8
                ,read "XYXXXIYX" :: Operator Word8
                ,read "ZIYXIZYI" :: Operator Word8
                ,read "IZIIIXZI" :: Operator Word8
                ,read "XYXYIZZZ" :: Operator Word8
                ]
             -- }}}
            ,bench "32 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 8)
                [read "ZYIYYXZY" :: Operator Word8
                ,read "IIYYZZXI" :: Operator Word8
                ,read "IYYIIYYI" :: Operator Word8
                ,read "XYIXXYIX" :: Operator Word8
                ,read "ZIZZYZXI" :: Operator Word8
                ,read "IIZXXIXI" :: Operator Word8
                ,read "IYXYIIZZ" :: Operator Word8
                ,read "YZIZXYYZ" :: Operator Word8
                ,read "IIYIYYZI" :: Operator Word8
                ,read "IXIYYZXI" :: Operator Word8
                ,read "YIXXYIXZ" :: Operator Word8
                ,read "XIZZIXZZ" :: Operator Word8
                ,read "YIYXZXXI" :: Operator Word8
                ,read "IXZYIYII" :: Operator Word8
                ,read "YYIZYZYZ" :: Operator Word8
                ,read "ZZZXIIXZ" :: Operator Word8
                ,read "IIZZIZII" :: Operator Word8
                ,read "XZYYYYZY" :: Operator Word8
                ,read "IXXZIIZY" :: Operator Word8
                ,read "IIIZYIXZ" :: Operator Word8
                ,read "XYXYXYYI" :: Operator Word8
                ,read "IYXYXZYY" :: Operator Word8
                ,read "YXXYXXXZ" :: Operator Word8
                ,read "ZIYZYXII" :: Operator Word8
                ,read "YZZYZZYX" :: Operator Word8
                ,read "IZZZYZZX" :: Operator Word8
                ,read "IYZYIZYZ" :: Operator Word8
                ,read "IYYZYXZI" :: Operator Word8
                ,read "YYIXYZIZ" :: Operator Word8
                ,read "YYXXZZIX" :: Operator Word8
                ,read "YYYIZZZI" :: Operator Word8
                ,read "YZYIIIXI" :: Operator Word8
                ]
             -- }}}
            ]
         -- 8 bits }}}
        ,bgroup "16 bits" -- {{{
            [bench "4 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 16)
                [read "IXYYYIZZYXYXIZXX" :: Operator Word16
                ,read "XIXYYXZZXZIZIIXY" :: Operator Word16
                ,read "IZZXZIXIXYZXYYIX" :: Operator Word16
                ,read "XZYXIIYXYZXXYXZY" :: Operator Word16
                ]
             -- }}}
            ,bench "8 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 16)
                [read "XXZXYZIXZZYIIYYX" :: Operator Word16
                ,read "YIYIIZXIIYXYZYIZ" :: Operator Word16
                ,read "ZYIXYYYIIYXZZIIY" :: Operator Word16
                ,read "XZZZYXYYXZYZXIIX" :: Operator Word16
                ,read "ZYIYZXZIIIIXXZXX" :: Operator Word16
                ,read "IXYZXZZXIYYZXIIX" :: Operator Word16
                ,read "XYIXIIYYZZYXIIYZ" :: Operator Word16
                ,read "IZIXIIZYYXIZIZXY" :: Operator Word16
                ]
             -- }}}
            ,bench "16 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 16)
                [read "YXIXYYZYZYIXXZYX" :: Operator Word16
                ,read "XYZYZYZXIYXYXZZI" :: Operator Word16
                ,read "IIXZIZZIZIZIYYXX" :: Operator Word16
                ,read "ZXZIZZYXXYYZIXZX" :: Operator Word16
                ,read "XIYXIIYYXYYZZYXY" :: Operator Word16
                ,read "XIZYXYYXIZIZIYZY" :: Operator Word16
                ,read "ZYXIIIIXXYZXZZIY" :: Operator Word16
                ,read "ZZYZYYZIZYXXZIZY" :: Operator Word16
                ,read "YXXZYIXZXXZIYIZY" :: Operator Word16
                ,read "YZXIYIXIIYIYYZXI" :: Operator Word16
                ,read "IZIIZIYIXXIIIXIY" :: Operator Word16
                ,read "IXYXIXYZYXYYIIYZ" :: Operator Word16
                ,read "YYIIYXYYXZXIXIIY" :: Operator Word16
                ,read "IZYXIXZYZYXYXYZI" :: Operator Word16
                ,read "YYZXIYXXXIZZIIYZ" :: Operator Word16
                ,read "ZYXIZYXIZZYXIIZY" :: Operator Word16
                ]
             -- }}}
            ,bench "32 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 16)
                [read "XIXIYZZZYXIYZIYZ" :: Operator Word16
                ,read "IZYIZYYIIIXZIXXZ" :: Operator Word16
                ,read "XZIYXIYXIYZXZXZY" :: Operator Word16
                ,read "YYIZIIIIXXZYZZYY" :: Operator Word16
                ,read "IZYYYIIXYYXXZYZX" :: Operator Word16
                ,read "ZIIZYZZYIYXIXIIY" :: Operator Word16
                ,read "XXXXIXZXIZXIIYYI" :: Operator Word16
                ,read "IXYYZIIZZIXYYZZX" :: Operator Word16
                ,read "YXXZZZYIZIXIZZIX" :: Operator Word16
                ,read "YXYIYZYXYIIXZZXZ" :: Operator Word16
                ,read "YZIIIXZXYYXIXIZI" :: Operator Word16
                ,read "YIZIXYZXYYZIIZXI" :: Operator Word16
                ,read "IZYZXXZIYIIYYYXX" :: Operator Word16
                ,read "IIXXIYIYIXXIXIYX" :: Operator Word16
                ,read "IYIZYYXZYZYIIIZI" :: Operator Word16
                ,read "YXYZYXZZXIYYIZXI" :: Operator Word16
                ,read "XZIXIYYXYZIYYXYZ" :: Operator Word16
                ,read "YXYZXXIZZIYXIZIZ" :: Operator Word16
                ,read "ZIXYZYYXZYZZZZZI" :: Operator Word16
                ,read "ZZZYIYXXIXZXZYZI" :: Operator Word16
                ,read "IYIZYZXYYYIYIZYX" :: Operator Word16
                ,read "YYYZXYIXXYZIXYXY" :: Operator Word16
                ,read "XXIYZYXZXIIIXYZX" :: Operator Word16
                ,read "IXIZZYZYYZXIIYXY" :: Operator Word16
                ,read "XYZZZIZXYXIXXXII" :: Operator Word16
                ,read "IYXYIZZZZZIYZIZI" :: Operator Word16
                ,read "YXYYXIZZZYYIXIXY" :: Operator Word16
                ,read "ZYXZXXIYYYXIYZIZ" :: Operator Word16
                ,read "IXXZXYXZYZZXYZXI" :: Operator Word16
                ,read "IYIXIXYIYYYZIXXI" :: Operator Word16
                ,read "XZXIZIZZXXYZYIZX" :: Operator Word16
                ,read "IYZIZIZZXZXZYYXZ" :: Operator Word16
                ]
             -- }}}
            ,bench "64 ops" $ -- {{{
                whnf (constructSubsystemCodeFromMeasurements 16)
                [read "XIYIXIYXYZZXXXXZ" :: Operator Word16
                ,read "XIZIXXYZXZYZZIZZ" :: Operator Word16
                ,read "IYXIIXZZZYXZXYYX" :: Operator Word16
                ,read "IZIYIYXIZIXIZIXX" :: Operator Word16
                ,read "IXZYYXYYYYXYYZZX" :: Operator Word16
                ,read "IIZIXZYIXXYZYZXX" :: Operator Word16
                ,read "ZYXXYIXIZXXIXIYZ" :: Operator Word16
                ,read "YXXXXXXZXYYZIXXY" :: Operator Word16
                ,read "ZIYIIZYXXZYIYZYY" :: Operator Word16
                ,read "XIZYXYYZIZZZYIYZ" :: Operator Word16
                ,read "IXZZIZXXIYXYXIIX" :: Operator Word16
                ,read "ZIYZZZZXIZXZXXZI" :: Operator Word16
                ,read "IIYZXXZXXXZXXIYY" :: Operator Word16
                ,read "YIYXXZYYIIZZIZII" :: Operator Word16
                ,read "XYXIXXYIIIYYYXXZ" :: Operator Word16
                ,read "XXYYZXYYIZXIZZYY" :: Operator Word16
                ,read "IIXZZIYXIYZIIZZI" :: Operator Word16
                ,read "XZIZIIIYXIZXXZYI" :: Operator Word16
                ,read "IXYIIYXZZIIXZIZX" :: Operator Word16
                ,read "XIXXIIIXXYXIYIYX" :: Operator Word16
                ,read "ZXIZXXYXYYIXIIYZ" :: Operator Word16
                ,read "ZZZXZYZXYIYZIXZZ" :: Operator Word16
                ,read "ZIYIIZIYXZYIIZYI" :: Operator Word16
                ,read "IXXYXIYZXXXXIZIX" :: Operator Word16
                ,read "ZYXIXIIIZXZZZZYI" :: Operator Word16
                ,read "IXYIXIIYYYZZIYZX" :: Operator Word16
                ,read "IYYIYIIYZXIYXIYZ" :: Operator Word16
                ,read "IYYIZZIXXIXXYXIX" :: Operator Word16
                ,read "XIXZYYYXXZZIZZIZ" :: Operator Word16
                ,read "IYXIYYZIZZZZXIXI" :: Operator Word16
                ,read "IZZZYXXXZIYIXXZX" :: Operator Word16
                ,read "IIYZIIYXIXXXIZYZ" :: Operator Word16
                ,read "YZYIYZZIYZIIIXXX" :: Operator Word16
                ,read "IXIXZZZYIZXYZZXX" :: Operator Word16
                ,read "ZXYXYXXZXIZYYXIY" :: Operator Word16
                ,read "ZXIZXZZXXZZZZYIY" :: Operator Word16
                ,read "YZXZZXIXZYYIYZIY" :: Operator Word16
                ,read "YZZIXIZXIYYXIYXI" :: Operator Word16
                ,read "ZZZIXIZIZZZYXXZZ" :: Operator Word16
                ,read "ZIYYIXZIZYZZZIYY" :: Operator Word16
                ,read "XZIXZXXIZZIYIXYY" :: Operator Word16
                ,read "YZZYYXYYXYXZZYZZ" :: Operator Word16
                ,read "ZZYIYZYZXIXZIIYY" :: Operator Word16
                ,read "IYYXZIZXXYYIZXXX" :: Operator Word16
                ,read "IXYXXYXIIXIIZYXX" :: Operator Word16
                ,read "YYIZIZZIZZZIZYYY" :: Operator Word16
                ,read "XXYXYXXYYIXXIXXX" :: Operator Word16
                ,read "XYIZXXYIZXYZIIYX" :: Operator Word16
                ,read "IXYIXZZYZYIIZXIX" :: Operator Word16
                ,read "YXIXXZIXZYYIZIZX" :: Operator Word16
                ,read "ZXXIYXXZIIXYIYXZ" :: Operator Word16
                ,read "IZXIXYXXZIZIZIZZ" :: Operator Word16
                ,read "IXXIIIZZYIXYYXZX" :: Operator Word16
                ,read "XZYZYXYYIIXIZZXY" :: Operator Word16
                ,read "IXXYYZZYIIYZXIYI" :: Operator Word16
                ,read "YYZIZXYYZZXIXYXI" :: Operator Word16
                ,read "ZIIYZXYYXYIYYXZI" :: Operator Word16
                ,read "XXZZXXIZYXYIZIYY" :: Operator Word16
                ,read "ZYXYZZIYXIYZIYYX" :: Operator Word16
                ,read "ZXYIXZXXZXZXIYIX" :: Operator Word16
                ,read "IIXXYZXXYXYZZXIY" :: Operator Word16
                ,read "YIZZIZZYXIZYZXZX" :: Operator Word16
                ,read "ZIXYZXIYYZIIIYIX" :: Operator Word16
                ,read "IIIYIZXYZXYZIZZY" :: Operator Word16
                ]
             -- }}}
            ]
         -- }}}
     -- }}} construction of subsytem code
        ]
    -- }}} Benchmarks
    ]

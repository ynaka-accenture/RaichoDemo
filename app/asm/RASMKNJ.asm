*---------------------------------------------------------------------
*  RASMKNJ   漢字混在文字列の SO/SI 整合検査  東西電力 電算部
*---------------------------------------------------------------------
*  H02.03 初版 (顧客名 DBCS 化のとき)
*  SO(X'0E') と SI(X'0F') の対応が崩れた文字列は帳票を壊すため
*  出力前に必ず本ルーチンを通すこと (電算部標準 3.7)
*  IF:  R1 -> ADDR(文字列), R0=長さ
*       戻り R15=0 整合 / 4 SO 過多 / 8 SI 過多 / 12 入れ子
*---------------------------------------------------------------------
RASMKNJ  CSECT
         STM   R14,R12,12(R13)
         BALR  R12,0
         USING *,R12
         LR    R9,R1               文字列アドレス
         LR    R8,R0               長さ
         SR    R7,R7               状態 0=SBCS 1=DBCS
KNJLOOP  DS    0H
         LTR   R8,R8
         BZ    KNJOWARI
         CLI   0(R9),X'0E'         SO か
         BNE   KNJSI
         LTR   R7,R7               既に DBCS 中なら入れ子
         BNZ   IRIKO
         LA    R7,1
         B     KNJNEXT
KNJSI    CLI   0(R9),X'0F'         SI か
         BNE   KNJNEXT
         LTR   R7,R7               SBCS 中の SI は過多
         BZ    SIKATA
         SR    R7,R7
KNJNEXT  LA    R9,1(R9)
         BCTR  R8,0
         B     KNJLOOP
KNJOWARI LTR   R7,R7               終端で DBCS 中なら SO 過多
         BNZ   SOKATA
         SR    R15,R15
         B     KNJMODO
SOKATA   LA    R15,4
         B     KNJMODO
SIKATA   LA    R15,8
         B     KNJMODO
IRIKO    LA    R15,12
KNJMODO  L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
         LTORG
         END   RASMKNJ

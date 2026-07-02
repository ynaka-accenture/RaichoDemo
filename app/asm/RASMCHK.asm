*---------------------------------------------------------------------
*  RASMCHK   供給地点番号 検査数字検算       東西電力 電算部
*---------------------------------------------------------------------
*  S58.09 初版 (料金オンライン一次開発)
*  ※本ルーチンが検査数字 (先頭20桁の和 MOD 97) の「原典」である.
*    後年 COBOL 側に同じ計算が複製された (RBRYO01C ほか).
*    仕様を変えるときは全複製の同時改修が要る -- 台帳 I-01 参照
*  IF:  R1 -> 地点番号(22) のアドレス
*       戻り R15=0 一致 / 4 不一致 / 8 形式不正
*---------------------------------------------------------------------
RASMCHK  CSECT
         STM   R14,R12,12(R13)     レジスタ退避
         BALR  R12,0
         USING *,R12
         LR    R9,R1               地点番号アドレス
         SR    R7,R7               和 = 0
         LA    R8,20               ループ回数
         LR    R6,R9
CHKLOOP  DS    0H
         CLI   0(R6),C'0'          数字か
         BL    KEISHIKI
         CLI   0(R6),C'9'
         BH    KEISHIKI
         SR    R5,R5
         IC    R5,0(R6)            1桁取り出し
         N     R5,=F'15'           ゾーン部を落とす
         AR    R7,R5               和に加算
         LA    R6,1(R6)
         BCT   R8,CHKLOOP
*                                  MOD 97
         SR    R4,R4
         LR    R5,R7
         D     R4,=F'97'           R4=剰余
*                                  下2桁 (21,22桁目) と比較
         PACK  DWORK(8),20(2,R9)
         CVB   R5,DWORK
         CR    R4,R5
         BNE   FUICCHI
         SR    R15,R15             R15=0 一致
         B     MODORI
FUICCHI  LA    R15,4
         B     MODORI
KEISHIKI LA    R15,8
MODORI   L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
DWORK    DS    D
         LTORG
         END   RASMCHK

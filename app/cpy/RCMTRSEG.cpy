      *----------------------------------------------------------------
      * RCMTRSEG  計器設備DB セグメント (MTRDBD)
      *   KEIKISEG (ルート): 計器資産の属性. 乗率の正本はここのみ
      *   TRTUSEG  (子)   : 取付履歴. 取外日=0 のツインが現行取付
      *----------------------------------------------------------------
       01  KEIKISEG-AREA.
           05  KS-KEIKI-NO     PIC X(10).
      *      検定満期は元号+YY (S58 設計のまま. E-01 窓割り)
           05  KS-KENTEI-GENGO PIC X(1).
           05  KS-KENTEI-YY    PIC 9(2).
           05  KS-KENTEI-MM    PIC 9(2).
           05  KS-JORITU       PIC 9(2)V9 COMP-3.
           05  KS-KETA-SU      PIC 9(1).
           05  KS-KOKAN-BI     PIC 9(8).
           05  KS-KISYU-CD     PIC X(6).
       01  TRTUSEG-AREA.
           05  TS-KEIKI-NO     PIC X(10).
           05  TS-TORITUKE-BI  PIC 9(8).
           05  TS-SPT-NO       PIC X(22).
           05  TS-TORIHAZUSI-BI PIC 9(8).

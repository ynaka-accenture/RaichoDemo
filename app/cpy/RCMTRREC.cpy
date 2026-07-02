      *----------------------------------------------------------------
      * RCMTRREC 計器マスタ (128 BYTE)
      *----------------------------------------------------------------
       01  MTR-REC.
           05  MTR-NO              PIC X(10).
           05  MTR-SPT-NO          PIC X(22).
      *      検定満期: 元号S/H/R. 旧レコードは元号空白で
      *      YY>=64昭和/未満平成の窓割り判定 (E-01)
           05  MTR-KENTEI-GENGO    PIC X(1).
           05  MTR-KENTEI-YY       PIC 9(2).
           05  MTR-KENTEI-MM       PIC 9(2).
           05  MTR-JORITU          PIC 9(2)V9 COMP-3.
           05  MTR-KETA-SU         PIC 9(1).
           05  MTR-KOKAN-BI        PIC 9(8).
           05  MTR-SETTI-BI        PIC 9(8).
           05  MTR-KISYU-CD        PIC X(6).
           05  FILLER              PIC X(66).

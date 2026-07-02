      *----------------------------------------------------------------
      * RCSHUREC 収納明細 (100 BYTE ESDS)
      *----------------------------------------------------------------
       01  SHU-REC.
           05  SHU-SPT-NO          PIC X(22).
           05  SHU-SEIKYU-YM       PIC 9(6).
           05  SHU-NYUKIN-BI       PIC 9(8).
           05  SHU-NYUKIN-GAKU     PIC S9(9) COMP-3.
           05  SHU-HOHO            PIC X(1).
           05  SHU-SHOKAI-NO       PIC X(20).
           05  SHU-KESHIKOMI-FLG   PIC X(1).
           05  FILLER              PIC X(37).

      *----------------------------------------------------------------
      * RCDMDMSG デマンド監視MQ電文 (96 BYTE)
      *   文字+PACKED+BINARY混在のためMQ CONVERT使用禁止 (G-04)
      *----------------------------------------------------------------
       01  DMD-MSG.
           05  DMD-SPT-NO          PIC X(22).
           05  DMD-JIKOKU          PIC X(12).
           05  DMD-DEMAND          PIC S9(5)V99 COMP-3.
           05  DMD-DENATU-KBN      PIC X(1).
           05  DMD-TUBAN           PIC 9(9) COMP.
      *      判定: 00正常 10警報 20超過 30遮断
           05  DMD-HANTEI          PIC X(2).
           05  DMD-KEIYAKU-DENRYOKU PIC S9(5) COMP-3.
           05  FILLER              PIC X(48).

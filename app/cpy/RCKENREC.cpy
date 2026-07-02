      *----------------------------------------------------------------
      * RCKENREC 検針実績 (128 BYTE)
      *----------------------------------------------------------------
       01  KEN-REC.
           05  KEN-SPT-NO          PIC X(22).
           05  KEN-NENGETU         PIC 9(6).
      *      検針日 YYMMDD 窓割り50 (E-03)
           05  KEN-KENSHIN-BI      PIC 9(6).
           05  KEN-ZEN-SIJISU      PIC 9(6).
           05  KEN-KON-SIJISU      PIC 9(6).
           05  KEN-SIYORYO         PIC S9(6) COMP-3.
      *      検針員: 旧データにスペース混入 (B-02)
           05  KEN-KENSHININ       PIC 9(5).
           05  KEN-KENSHIN-KBN     PIC X(1).
           05  KEN-KOKAN-FLG       PIC X(1).
           05  KEN-BIKO            PIC X(30).
           05  FILLER              PIC X(41).

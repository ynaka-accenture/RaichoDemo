      *----------------------------------------------------------------
      * RCZENGIN 全銀振替結果明細 (120 BYTE 固定)
      *----------------------------------------------------------------
       01  ZG-REC.
           05  ZG-DATA-KBN         PIC X(1).
           05  ZG-GINKO            PIC 9(4).
           05  ZG-SITEN            PIC 9(3).
           05  ZG-YOKIN-SYU        PIC 9(1).
           05  ZG-KOZA             PIC 9(7).
      *      受取人名: 半角カナ (A-04)
           05  ZG-UKETORI-KANA     PIC X(30).
      *      金額: 訂正時は末尾符号オーバーパンチ J-R (B-01)
           05  ZG-KINGAKU          PIC S9(10).
           05  ZG-SHINKI-CD        PIC X(1).
           05  ZG-SHOKAI-NO        PIC X(20).
           05  ZG-FURIKAE-KEKKA    PIC X(1).
           05  ZG-FURIKAE-BI       PIC 9(8).
           05  FILLER              PIC X(34).

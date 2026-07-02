      *----------------------------------------------------------------
      * RCTANKA 単価表 (80 BYTE 順編成)
      *   ※プログラム内テーブルと二重管理・内蔵側が正 (F-03)
      *----------------------------------------------------------------
       01  TAN-REC.
           05  TAN-SYUBETU         PIC X(2).
           05  TAN-TEKIYO-KAISI    PIC 9(8).
           05  TAN-KIHON-TANKA     PIC S9(5)V99 COMP-3.
           05  TAN-DAN1-TANKA      PIC S9(3)V99 COMP-3.
           05  TAN-DAN2-TANKA      PIC S9(3)V99 COMP-3.
           05  TAN-DAN3-TANKA      PIC S9(3)V99 COMP-3.
      *      燃調単価: 銭2桁保持・負値あり (F-01)
           05  TAN-NENCHO-TANKA    PIC S9(2)V99 COMP-3.
           05  TAN-SAIENE-TANKA    PIC S9(2)V99 COMP-3.
           05  TAN-ZEI-RITU        PIC 9(2)V9 COMP-3.
           05  TAN-ZEI-KBN         PIC X(1).
           05  FILLER              PIC X(48).

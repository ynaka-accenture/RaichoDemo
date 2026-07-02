      *----------------------------------------------------------------
      * RCRYOMAP  料金照会画面 シンボリックマップ (RRYO01M)
      *----------------------------------------------------------------
       01  RYO01MI.
           05  RYI-SPT         PIC X(22).
           05  FILLER          PIC X(58).
       01  RYO01MO REDEFINES RYO01MI.
           05  RYO-O-SPT       PIC X(22).
           05  RYO-O-YM        PIC X(6).
           05  RYO-O-GOKEI     PIC X(11).
           05  RYO-O-KIHON     PIC X(8).
           05  RYO-O-ZEI       PIC X(8).
           05  RYO-O-FLG       PIC X(1).
           05  RYO-O-MSG       PIC X(24).

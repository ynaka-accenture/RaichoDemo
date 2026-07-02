#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
gen_mtrdb.py — IMS 計器設備DB (MTRDB) の初期ロード生成
既存 MTRMAST.dat から導出する (IMS が正本、MTRMAST は抽出物という
関係を成立させるため、全 128 バイトが DB から復元可能であること)。
unload 形式: 固定 80B SEQUENTIAL、階層順 (R の直後にその T 群)
  'R' + 計器No(10)+元号(1)+YY(2)+MM(2)+乗率C3(2)+桁(1)+交換日(8)
      + 機種(6) = 1+32
  'T' + 計器No(10)+取付日(8)+供給地点(22)+取外日(8) = 1+48
過去取付履歴は計器No由来の決定的擬似乱数で 0-2 件付与
(取外日=次の取付日、現行ツインの取外日=00000000)。
"""
import random, os
BASE = os.path.join(os.path.dirname(__file__), '..')
P = f'{BASE}/app/data/portable'

def main():
    d = open(f'{P}/MTRMAST.dat', 'rb').read()
    spts = []
    k = open(f'{P}/KYKMAST.dat', 'rb').read()
    for r in range(0, len(k), 320):
        if k[r:r+1] != b'\x00':
            spts.append(k[r:r+22])
    roots = twins = 0
    with open(f'{P}/MTRDB.dat', 'wb') as f:
        for r in range(0, len(d), 128):
            b = d[r:r+128]
            mtr_no, spt = b[0:10], b[10:32]
            gengo_ym, joritu = b[32:37], b[35:37]
            # RCMTRREC 準拠: G@32 YY@33 MM@35 JORITU@37(2B) KETA@39
            #               KOKAN@40 SETTI@48 KISYU@56
            root = (b'R' + b[0:10] + b[32:33] + b[33:35] + b[35:37]
                    + b[37:39] + b[39:40] + b[40:48] + b[56:62])
            f.write(root.ljust(80, b' '))
            roots += 1
            setti = b[48:56]
            rnd = random.Random(int(mtr_no[3:10]) * 7 + 3)
            n_past = rnd.randint(0, 2)
            dates = sorted(rnd.randint(19850101, 20091231)
                           for _ in range(n_past))
            prev = []
            for i, td in enumerate(dates):
                tori = dates[i+1] if i+1 < len(dates) \
                    else int(setti.decode())
                pspt = spts[rnd.randrange(len(spts))]
                prev.append((f'{td:08d}'.encode(), pspt,
                             f'{tori:08d}'.encode()))
            for td, pspt, tz in prev:
                f.write((b'T' + mtr_no + td + pspt + tz)
                        .ljust(80, b' '))
                twins += 1
            f.write((b'T' + mtr_no + setti + spt + b'00000000')
                    .ljust(80, b' '))
            twins += 1
    print(f'MTRDB: roots={roots:,} twins={twins:,}')

if __name__ == '__main__':
    main()

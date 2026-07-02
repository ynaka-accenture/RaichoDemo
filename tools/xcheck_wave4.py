#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""xcheck_wave4.py — RBRYO00C(外税), RBMTR01C(E-01窓割り), RBBKU01C の検算"""
import sys, os
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def unpack(b):
    h = b.hex(); v = int(h[:-1])
    return -v if h[-1] in 'db' else v

def rhu(n, d): return (n * 2 + d) // (d * 2)

def main():
    ok = True
    # ---- RBRYO00C: 旧方式(外税3%)の全件再計算 ----
    ken = open(f'{P}/KENFILE.dat', 'rb').read()
    old = open(f'{P}/RYOOLD.dat', 'rb').read()
    oi = ng = 0
    for r in range(0, len(ken), 128):
        b = ken[r:r+128]
        if int(b[22:28]) != 202406:
            continue
        u = abs(unpack(b[46:50]))
        kihon = 600
        juryo = (2100 * u) // 100
        zei = rhu((kihon + juryo) * 3, 100)
        gokei = kihon + juryo + zei
        o = old[oi*100:(oi+1)*100]; oi += 1
        if (int(o[34:41]), int(o[41:48]), int(o[48:55]),
                int(o[55:64])) != (kihon, juryo, zei, gokei):
            ng += 1
    ok1 = ng == 0 and oi == len(old) // 100
    ok &= ok1
    print(f'XCHECK OLD: RECS={oi} NG={ng} -> {"OK" if ok1 else "NG"}')
    # ---- RBMTR01C: 窓割り・期限判定の再現 ----
    mtr = open(f'{P}/MTRMAST.dat', 'rb').read()
    kij = 202607
    kire = majika = mado_s = mado_h = 0
    for r in range(0, len(mtr), 128):
        b = mtr[r:r+128]
        g = b[32:33].decode(); yy = int(b[33:35]); mm = int(b[35:37])
        if g == 'R': y = 2018 + yy
        elif g == 'H': y = 1988 + yy
        elif g == 'S': y = 1925 + yy
        else:
            if yy >= 64: y = 1925 + yy; mado_s += 1
            else: y = 1988 + yy; mado_h += 1
        manki = (y + 10) * 100 + mm
        if manki < kij: kire += 1
        else:
            g6 = kij + 6
            if g6 % 100 > 12: g6 += 88
            if manki <= g6: majika += 1
    kgn = open(f'{P}/MTRKIGN.dat', 'rb').read()
    ok2 = (len(kgn) // 48 == kire + majika and mado_s == 441
           and mado_h == 459)
    ok &= ok2
    print(f'XCHECK MTR: KIRE={kire} MAJIKA={majika} 窓S/H='
          f'{mado_s}/{mado_h} KIGN={len(kgn)//48} '
          f'-> {"OK" if ok2 else "NG"}')
    # ---- RBBKU01C: バイト同一+制御レコード ----
    src = open(f'{P}/KYKMAST.dat', 'rb').read()
    bkp = open(f'{P}/KYKBKUP.dat', 'rb').read()
    ctl = open(f'{P}/KYKBKCTL.dat', 'rb').read()
    ksum = 0
    for r in range(0, len(src), 320):
        if src[r:r+1] == b'\x00':
            continue
        ksum += int(src[r+22:r+32])
        while ksum >= 9000000000000:
            ksum -= 9000000000000
    ok3 = (src == bkp and int(ctl[0:7]) == len(src) // 320
           and int(ctl[7:20]) == ksum)
    ok &= ok3
    print(f'XCHECK BKU: 同一={src == bkp} 件数={int(ctl[0:7])} '
          f'検査和一致={int(ctl[7:20]) == ksum} '
          f'-> {"OK" if ok3 else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main())

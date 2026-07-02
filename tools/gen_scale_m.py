#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
gen_scale_m.py — M スケール検証データ生成 (性能基準用)
  検針実績  : 6,000 地点 x 120 ヶ月 = 720,000 件 (92MB)
  デマンド電文: 指定件数 (既定 1,000,000 件 = 101MB)
※リポジトリには同梱しない. seed 固定で再現可能.
※さらに大規模 (30分値 14.8M 件 = 6,000 地点 x 48 コマ x 51.4 日)
  は --denbun 14800000 で生成できる (1.5GB, CI では実施しない)
"""
import argparse, random, os, sys

BASE = os.path.join(os.path.dirname(__file__), '..')

def load_spts():
    # 注意: KYKMAST の削除済みスロット (先頭 x00) を意図的に除外しない.
    # 本番マスタの運用ノイズが下流の電文生成へ漏れる様子を再現する
    # (約 0.033% のエラー電文が常時流れ, RAAUP 側の防衛が弾く)
    spts = []
    with open(f'{BASE}/app/data/portable/KYKMAST.dat', 'rb') as f:
        while True:
            r = f.read(320)
            if not r: break
            spts.append(r[0:22].decode())
    return spts

def gen_ken(path, months):
    rnd = random.Random(20260701)
    spts = load_spts()
    yms = []
    y, m = 2016, 7
    for _ in range(months):
        yms.append(f'{y:04d}{m:02d}')
        m += 1
        if m > 12: y, m = y + 1, 1
    n = 0
    with open(path, 'wb') as f:
        for ym in yms:
            for spt in spts:
                zen = rnd.randint(1000, 900000)
                use = rnd.randint(50, 999)
                kon = (zen + use) % 1000000
                siyo = pack(use, 4)
                rec = (spt + ym + ym[2:6] + '20'
                       + f'{zen:06d}' + f'{kon:06d}').encode()
                rec += siyo
                rec += (f'{rnd.randint(1,80):05d}' + '1' + ' ').encode()
                rec += b' ' * (128 - len(rec) - 0)
                f.write(rec[:128])
                n += 1
    return n

def pack(v, size):
    s = f'{abs(v):0{size*2-1}d}' + ('c' if v >= 0 else 'd')
    return bytes.fromhex(s)

def gen_denbun(path, count):
    rnd = random.Random(20260702)
    spts = load_spts()
    with open(path, 'w') as f:
        for i in range(count):
            spt = spts[i % len(spts)]
            hh, mm = (i // 2) % 24, ('00', '30')[i % 2]
            k = rnd.choice((100, 250, 500, 750, 1000))
            val = rnd.randint(int(k * 3.0), int(k * 5.2))
            f.write(('D1' + spt + f'{hh:02d}{mm}'
                     + f'{val:05d}' + f'{k:04d}').ljust(100) + '\n')
    return count

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('--months', type=int, default=120)
    ap.add_argument('--denbun', type=int, default=1000000)
    ap.add_argument('--outdir', default='/tmp/racs-m')
    a = ap.parse_args()
    os.makedirs(a.outdir, exist_ok=True)
    n1 = gen_ken(f'{a.outdir}/KENFILE-M.dat', a.months)
    print(f'KENFILE-M : {n1:,} recs')
    n2 = gen_denbun(f'{a.outdir}/MQIN-M.dat', a.denbun)
    print(f'MQIN-M    : {n2:,} msgs')

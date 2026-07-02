#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
gtf_view.py — 統合デバッグビュー
  cobc -ftraceall の文トレース (spool/gtf/cobol.trc) と
  各スタブの GTF API トレース (spool/gtf/*.trc) を突合し、
  「どの COBOL 文の CALL で、どの CICS/DL-I/SQL/MQ が走ったか」を
  単一の時系列絵巻にする。

相関原理: 同一プロセス単一スレッドのため、文トレース中の
  スタブ CALL の出現順 == GTF 通番順。CALL 文の呼び先は
  トレースに出ないので、Line 番号で生成ソース原文を引いて判定する。

使い方:
  python3 tools/gtf_view.py [--prog RASGN00C] [--full] \
      [--out spool/gtf/annotated.txt]
  既定: スタブ (RXCICSTB/CBLTDLI/RXDB2TB/RUMQSUB) とドライバの
        内部文は折り畳み、業務プログラムの文+GTF のみ表示
"""
import argparse, glob, os, re, sys

BASE = os.path.join(os.path.dirname(__file__), '..')
GTFD = f'{BASE}/spool/gtf'
STUBS = {'RXCICSTB', 'CBLTDLI', 'RXDB2TB', 'RUMQSUB'}
HIDDEN = STUBS | {'TESTCICS', 'TESTIMS'}
SRC_DIRS = ['tests/cics-gen', 'tests/sql-gen', 'app/cbl', 'tests']

def build_source_index():
    """PROGRAM-ID -> (path, lines[])"""
    idx = {}
    for d in SRC_DIRS:
        for p in glob.glob(f'{BASE}/{d}/*.cbl'):
            try:
                lines = open(p, encoding='utf-8').read().split('\n')
            except UnicodeDecodeError:
                continue
            for ln in lines[:30]:
                m = re.search(r'PROGRAM-ID\.\s+(\S+?)\.', ln)
                if m and m.group(1) not in idx:
                    idx[m.group(1)] = (p, lines)
                    break
    return idx

def load_gtf():
    recs = []
    for p in glob.glob(f'{GTFD}/*.trc'):
        if p.endswith('cobol.trc'):
            continue
        for ln in open(p):
            m = re.match(r'GTF (\d{9}) (.*)', ln.rstrip())
            if m:
                recs.append((int(m.group(1)), m.group(2).rstrip()))
    recs.sort()
    return recs

TRC_RE = re.compile(
    r"Program-Id:\s+(\S+)\s+"
    r"(?:(Entry|Exit|Paragraph|Section):\s*(\S+)|(\S.*?))\s+"
    r"Line:\s+(\d+)")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--prog', help='この業務プログラムのみ表示')
    ap.add_argument('--full', action='store_true',
                    help='スタブ/ドライバ内部も表示')
    ap.add_argument('--out')
    a = ap.parse_args()
    src = build_source_index()
    gtf = load_gtf()
    gi = 0
    out = open(a.out, 'w') if a.out else sys.stdout
    shown = skipped = matched = 0
    for raw in open(f'{GTFD}/cobol.trc'):
        m = TRC_RE.match(raw.strip())
        if not m:
            continue
        prog, kind, name, verb, line = m.groups()
        line = int(line)
        is_call = (verb or '').strip().startswith('CALL')
        target = None
        text = ''
        if prog in src:
            path, lines = src[prog]
            if 0 < line <= len(lines):
                text = lines[line - 1][6:72].strip()
        if is_call:
            cm = re.search(r"CALL\s+'(\w+)'", text)
            if cm:
                target = cm.group(1)
        # 表示判定
        visible = a.full or (prog not in HIDDEN)
        if a.prog and prog != a.prog:
            visible = False
        if visible:
            shown += 1
            if kind:  # Entry/Paragraph/Section
                out.write(f'{prog:9} {line:5} == {kind} {name} ==\n')
            else:
                out.write(f'{prog:9} {line:5}    {text or verb}\n')
        else:
            skipped += 1
        # スタブ CALL に GTF を対応付け (可視性に関わらず消費)
        if is_call and target in STUBS and gi < len(gtf):
            seq, body = gtf[gi]
            gi += 1
            matched += 1
            if visible or a.full or (a.prog is None):
                out.write(f'          >>>>> GTF#{seq:09d} {body}\n')
    out.write(f'\n---- 文={shown} 折畳={skipped} '
              f'GTF対応={matched}/{len(gtf)} ----\n')
    if a.out:
        out.close()
        print(f'annotated -> {a.out} (GTF {matched}/{len(gtf)} 対応)')

if __name__ == '__main__':
    main()

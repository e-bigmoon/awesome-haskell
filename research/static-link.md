# バイナリ (静的リンク) の作り方

- alpine (buxybox) で動かしたい
- [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell/tree/master) で実験

## ファイルサイズまとめ

名前 | 動的リンク | 静的リンク | -optc-Os | --split-objs | UPX
-----|---------|-----------|----------|-------------|-------
fixpoint | 13.7M | 14.3M | 14.3M | 6.3M (55.94% 減) | 1.2M (91.60% 減)
liquid | 92.2M | 93.5M | 93.5M | 77.8M (16.79% 減) | 9.4M (89.95% 減)
target | 69.7M | 70.3M | 70.3M | 63.3M (9.96% 減) | 7.4M (89.47% 減)
合計 | 175.6M | 178.1M | 178.1M | 147.4M (17.24% 減) | 18M (89.19% 減)

- `-optc-Os`: 効果無し
- `--split-objs`: 場合によっては大幅なファイルサイズ減につながる
- `UPX`: 絶大な効果があるが、バイナリサイズに応じてかなりの時間がかかる

## バイナリ (動的リンク)

```dockerfile
FROM alpine

# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev

# GRAB A RECENT BINARY OF STACK
RUN curl -sSL https://get.haskellstack.org/ | sh

# COMPILE
RUN git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
WORKDIR liquidhaskell
RUN git checkout master
RUN stack --local-bin-path /sbin install --system-ghc

# SHOW INFORMATION
RUN ldd /sbin/fixpoint || true
RUN du -hs /sbin/fixpoint
RUN ldd /sbin/liquid || true
RUN du -hs /sbin/liquid
RUN ldd /sbin/target || true
RUN du -hs /sbin/target
```

### ログ

```bash
Step 9/14 : RUN ldd /sbin/fixpoint || true
 ---> Running in aff89ae2460f
	/lib/ld-musl-x86_64.so.1 (0x7f15a7d62000)
	libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x7f15a7a10000)
	libgmp.so.10 => /usr/lib/libgmp.so.10 (0x7f15a77ac000)
	libffi.so.6 => /usr/lib/libffi.so.6 (0x7f15a75a4000)
	libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x7f15a7392000)
	libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7f15a7d62000)
 ---> b87bc4ec86cb
Removing intermediate container aff89ae2460f
Step 10/14 : RUN du -hs /sbin/fixpoint
 ---> Running in ddbe9b22bf6c
13.7M	/sbin/fixpoint
 ---> e87cd0b1dea9
Removing intermediate container ddbe9b22bf6c
Step 11/14 : RUN ldd /sbin/liquid || true
 ---> Running in 7daa1506e651
	/lib/ld-musl-x86_64.so.1 (0x7fe790d3a000)
	libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x7fe7909e8000)
	libgmp.so.10 => /usr/lib/libgmp.so.10 (0x7fe790784000)
	libffi.so.6 => /usr/lib/libffi.so.6 (0x7fe79057c000)
	libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x7fe79036a000)
	libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7fe790d3a000)
 ---> d679da3d6031
Removing intermediate container 7daa1506e651
Step 12/14 : RUN du -hs /sbin/liquid
 ---> Running in e078890b21be
92.2M	/sbin/liquid
 ---> 8231725ac41b
Removing intermediate container e078890b21be
Step 13/14 : RUN ldd /sbin/target || true
 ---> Running in 48c15fdc0ee7
	/lib/ld-musl-x86_64.so.1 (0x7f4ade129000)
	libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x7f4adddd7000)
	libgmp.so.10 => /usr/lib/libgmp.so.10 (0x7f4addb73000)
	libffi.so.6 => /usr/lib/libffi.so.6 (0x7f4add96b000)
	libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x7f4add759000)
	libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7f4ade129000)
 ---> 4cbadc364338
Removing intermediate container 48c15fdc0ee7
Step 14/14 : RUN du -hs /sbin/target
 ---> Running in 4a9dc4d796bb
69.7M	/sbin/target
 ---> 055259003cd6
```

## バイナリ (静的リンク)

```dockerfile
FROM alpine
# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev
# GRAB A RECENT BINARY OF STACK
RUN curl -sSL https://get.haskellstack.org/ | sh
# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/6.3.0/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
# COMPILE
RUN git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
WORKDIR liquidhaskell
RUN git checkout master
RUN stack --local-bin-path /sbin install --system-ghc --ghc-options '-optl-static -fPIC'
# SHOW INFORMATION
RUN ldd /sbin/fixpoint || true
RUN du -hs /sbin/fixpoint
RUN ldd /sbin/liquid || true
RUN du -hs /sbin/liquid
RUN ldd /sbin/target || true
RUN du -hs /sbin/target
```

### ログ

```bash
Step 12/17 : RUN ldd /sbin/fixpoint || true
 ---> Running in f7a6acfcda18
ldd: /sbin/fixpoint: Not a valid dynamic program
 ---> 761b6dc7b96e
Removing intermediate container f7a6acfcda18
Step 13/17 : RUN du -hs /sbin/fixpoint
 ---> Running in 8c8f595cef8e
14.3M	/sbin/fixpoint
 ---> 4c0d2ef2fd10
Removing intermediate container 8c8f595cef8e
Step 14/17 : RUN ldd /sbin/liquid || true
 ---> Running in 7c193b01d21a
ldd: /sbin/liquid: Not a valid dynamic program
 ---> 2d4ab39a3dcd
Removing intermediate container 7c193b01d21a
Step 15/17 : RUN du -hs /sbin/liquid
 ---> Running in 210676e34526
93.5M	/sbin/liquid
 ---> cd78ff84786c
Removing intermediate container 210676e34526
Step 16/17 : RUN ldd /sbin/target || true
 ---> Running in 37f5b3aa9998
ldd: /sbin/target: Not a valid dynamic program
 ---> 16a9519c890d
Removing intermediate container 37f5b3aa9998
Step 17/17 : RUN du -hs /sbin/target
 ---> Running in 9c12a21d636b
70.3M	/sbin/target
 ---> 1b4c774d19da
```

## -optc-Os

```dockerfile
FROM alpine
# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev
# GRAB A RECENT BINARY OF STACK
RUN curl -sSL https://get.haskellstack.org/ | sh
# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/6.3.0/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
# COMPILE
RUN git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
WORKDIR liquidhaskell
RUN git checkout master
RUN stack --local-bin-path /sbin install --system-ghc --ghc-options '-optl-static -fPIC -optc-Os'
# SHOW INFORMATION
RUN ldd /sbin/fixpoint || true
RUN du -hs /sbin/fixpoint
RUN ldd /sbin/liquid || true
RUN du -hs /sbin/liquid
RUN ldd /sbin/target || true
RUN du -hs /sbin/target
```

### ログ

```bash
Step 12/17 : RUN ldd /sbin/fixpoint || true
 ---> Running in ad001593cba1
ldd: /sbin/fixpoint: Not a valid dynamic program
 ---> b7249e857a60
Removing intermediate container ad001593cba1
Step 13/17 : RUN du -hs /sbin/fixpoint
 ---> Running in 9eb73b58da78
14.3M	/sbin/fixpoint
 ---> 6c7d3b09651f
Removing intermediate container 9eb73b58da78
Step 14/17 : RUN ldd /sbin/liquid || true
 ---> Running in 5452e4c09afb
ldd: /sbin/liquid: Not a valid dynamic program
 ---> 47117ced6a4a
Removing intermediate container 5452e4c09afb
Step 15/17 : RUN du -hs /sbin/liquid
 ---> Running in 859ce30e554d
93.5M	/sbin/liquid
 ---> dc56c4a1827e
Removing intermediate container 859ce30e554d
Step 16/17 : RUN ldd /sbin/target || true
 ---> Running in 0ce446b21d1c
ldd: /sbin/target: Not a valid dynamic program
 ---> 6bce6b7a06b8
Removing intermediate container 0ce446b21d1c
Step 17/17 : RUN du -hs /sbin/target
 ---> Running in a5f7dc80af6d
70.3M	/sbin/target
 ---> 37b7550033e9
```

## --split-objs

```dockerfile
FROM alpine AS build-lh
# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev
# GRAB A RECENT BINARY OF STACK
RUN curl -sSL https://get.haskellstack.org/ | sh
# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/6.3.0/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
# COMPILE
RUN git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
WORKDIR liquidhaskell
RUN git checkout master
RUN stack --local-bin-path /sbin install --system-ghc --ghc-options '-optl-static -fPIC -optc-Os' --split-objs
# SHOW INFORMATION
RUN ldd /sbin/fixpoint || true
RUN du -hs /sbin/fixpoint
RUN ldd /sbin/liquid || true
RUN du -hs /sbin/liquid
RUN ldd /sbin/target || true
RUN du -hs /sbin/target
```

### ログ

```bash
Step 12/17 : RUN ldd /sbin/fixpoint || true
 ---> Running in 5092b4f398c8
ldd: /sbin/fixpoint: Not a valid dynamic program
 ---> 2799a97f1381
Removing intermediate container 5092b4f398c8
Step 13/17 : RUN du -hs /sbin/fixpoint
 ---> Running in c0c64f74e657
6.3M	/sbin/fixpoint
 ---> b1235c1e4ab7
Removing intermediate container c0c64f74e657
Step 14/17 : RUN ldd /sbin/liquid || true
 ---> Running in 42b1d4cb5eb7
ldd: /sbin/liquid: Not a valid dynamic program
 ---> 9325538e3d02
Removing intermediate container 42b1d4cb5eb7
Step 15/17 : RUN du -hs /sbin/liquid
 ---> Running in 053a194f4265
77.8M	/sbin/liquid
 ---> abbc235463ad
Removing intermediate container 053a194f4265
Step 16/17 : RUN ldd /sbin/target || true
 ---> Running in f6011812a33a
ldd: /sbin/target: Not a valid dynamic program
 ---> 5a74c7721c20
Removing intermediate container f6011812a33a
Step 17/17 : RUN du -hs /sbin/target
 ---> Running in b2a19406aef6
63.3M	/sbin/target
 ---> a166d538cf1b
Removing intermediate container b2a19406aef6
Successfully built a166d538cf1b
Successfully tagged waddlaw/liquidhaskell:latest
```

## UPX

- [UPX](https://upx.github.io/)
- [UPX on github](https://github.com/upx/upx)
- [docker-upx](https://github.com/lalyos/docker-upx)

```dockerfile
FROM alpine AS build-lh
# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev
# GRAB A RECENT BINARY OF STACK
RUN curl -sSL https://get.haskellstack.org/ | sh
# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/6.3.0/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
# COMPILE
RUN git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
WORKDIR liquidhaskell
RUN git checkout master
RUN stack --local-bin-path /sbin install --system-ghc --ghc-options '-optl-static -fPIC -optc-Os' --split-objs
# SHOW INFORMATION
RUN ldd /sbin/fixpoint || true
RUN du -hs /sbin/fixpoint
RUN ldd /sbin/liquid || true
RUN du -hs /sbin/liquid
RUN ldd /sbin/target || true
RUN du -hs /sbin/target
```

### ログ

```bash
Step 17/22 : RUN ldd /sbin/fixpoint || true
 ---> Running in 188f6ff6e176
ldd: /sbin/fixpoint: Not a valid dynamic program
 ---> 63909cfe9363
Removing intermediate container 188f6ff6e176
Step 18/22 : RUN du -hs /sbin/fixpoint
 ---> Running in ab877c35faa8
1.2M	/sbin/fixpoint
 ---> 54a2b57c7363
Removing intermediate container ab877c35faa8
Step 19/22 : RUN ldd /sbin/liquid || true
 ---> Running in 9e2746e498f5
ldd: /sbin/liquid: Not a valid dynamic program
 ---> 2e86f2d0e11d
Removing intermediate container 9e2746e498f5
Step 20/22 : RUN du -hs /sbin/liquid
 ---> Running in 2295a26ef8b8
9.4M	/sbin/liquid
 ---> 203c78f44650
Removing intermediate container 2295a26ef8b8
Step 21/22 : RUN ldd /sbin/target || true
 ---> Running in c74b46020927
ldd: /sbin/target: Not a valid dynamic program
 ---> e687c65f3f75
Removing intermediate container c74b46020927
Step 22/22 : RUN du -hs /sbin/target
 ---> Running in 3ccc995ed0ad
7.4M	/sbin/target
 ---> 6fadae7f4c16
```


## 参考リンク
- [STATIC COMPILATION WITH STACK](https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack)
- [stackとdockerでHaskellプログラムを静的リンクする](https://www.ishiy.xyz/posts/2016-02-28-haskell-docker.html)
- [Package Details: stack-static 1.5.1-1](https://aur.archlinux.org/packages/stack-static/)
- [build-static.sh](https://github.com/fpco/stack-docker-image-build/blob/master/build-static.sh)

# バイナリ (静的リンク) の作り方

- alpine (buxybox)で動かしたい
- [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell/tree/master) で実験

## バイナリ (動的リンク) の作り方

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

```
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

## ファイルサイズ

名前 | 動的リンク | 静的リンク
------|--------|--------
fixpoint | 13.7M | 
liquid | 92.2M |
target | 69.7M |



## 参考リンク
- [STATIC COMPILATION WITH STACK](https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack)
- [stackとdockerでHaskellプログラムを静的リンクする](https://www.ishiy.xyz/posts/2016-02-28-haskell-docker.html)
- [Package Details: stack-static 1.5.1-1](https://aur.archlinux.org/packages/stack-static/)

!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
! 操作系统大小：3 x 64KB = 192KB
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.
!
! bootsect.s由BIOS加载到0x7c00处开始执行，它将自己拷贝到0x90000开始的内存，
! 然后跳转到新的bootsect.s位置继续执行。
!
! 然后使用BIOS中断加载setup到自己后面（从0x90200开始），以及加载系统到0x10000处。
!
! 注意！ 当前系统最大为8x64KB=512KB，即使到了将来，这也应该不会有问题。
!
! 加载器已经实现得尽可能简单，多次读磁盘错误会导致死循环，需要手动重启。
! 通过每次尽可能多的读取扇区，加载速度会很快。


.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

# 启动程序大小为4个扇区，最大为2KB
SETUPLEN = 4				! nr of setup-sectors
# 引导程序起始运行位置CS:IP=0x07c0:0x0000
BOOTSEG  = 0x07c0			! original address of boot-sector
# 新的引导程序起始运行位置CS:IP=0x9000:0x0000
INITSEG  = 0x9000			! we move boot here - out of the way
# 启动程序起始位置CS:IP=0x9020:0x0000
SETUPSEG = 0x9020			! setup starts here
# 系统起始运行位置CS:IP=0x1000:0x0000
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
# 系统结束位置
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading


! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
! 根设备：指定启动设备
ROOT_DEV = 0x306

! 程序入口
entry _start
_start:
	mov	ax,#BOOTSEG
	mov	ds,ax		! ds = 0x07c0
	mov	ax,#INITSEG
	mov	es,ax		! es = 0x9000
	mov	cx,#256		! 加载引导扇区，共512B
	sub	si,si
	sub	di,di
	rep
	movw
	jmpi	go,INITSEG	! 立即跳转到0x9000:go位置执行
go:	mov	ax,cs
	mov	ds,ax		! ds = 0x9000
	mov	es,ax		! es = 0x9000

! 为什么要在此处初始化栈？
! put stack at 0x9ff00.
! 建立栈：SS:SP=0x9000:0xFF00
	mov	ss,ax
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.
! 直接加载setup扇区到boot程序后面
! 注意：es已经设置完毕。

! BIOS中断调用0x13:低端磁盘服务
! AH=02h 读磁盘
! ES:BX 内存位置
! DH 驱动器号80h/81h;			DL 磁头号0~1
! CH 磁道号0-1023, 高两位为CL[8,9];	CL 扇区号1-17
! AL 读取的扇区数量1~80h
! 返回：AL 已经读区的扇区数,AH 0x00 Carry=1无错误
load_setup:
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0 (跳过bootsect)
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
	int	0x13			! read it
	jnc	ok_load_setup		! ok - continue
! INT 13h AH=00h 复位磁盘
	mov	dx,#0x0000		! 设置驱动器号
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup		! 重新尝试读区setup，如果失败，死循环

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track
! 读区磁盘驱动器参数，尤其是扇区数，磁道数
! INT 13h AH=08h 读取磁盘参数
! DL 驱动器号
! 返回：
! ES:DI 指向参数表
! DH 最大磁头数；			DL 当前驱动器编号
! CH 最大磁道数；			CL 最大扇区数
! BL 磁盘类型 02h=1.2MB 04h=1.44MB
	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs
	mov	sectors,cx		! sectors变量存放扇区数
	mov	ax,#INITSEG
	mov	es,ax			! ES = 0x9000

! Print some inane message
! 打印信息

! INT 10h AH=03h 读取光标位置
! BH 需要返回光标的页
! 返回：
! DH 光标行位置；			DL 光标列位置
! CH 光标底部扫描线；			CL 光标顶部扫描线
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh			! BH=00h 第0页
	int	0x10
	
! INT 10h AH=13h 写字符串
! ES:BP 指向字符串
! 返回：
! CX 字符长度
! DH 光标行位置；			DL 光标列位置
! BL 显示属性				AL 显示模式 01h 仅字符，更新光标
	mov	cx,#24			! 字符数
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)
! 开始加载系统
	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000 (ES = 0x1000)
	call	read_it		! 调用读程序
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.
!
! 接下来检查根设备。如果根设备已经定义(!=0)，则直接使用指定的设备。
! 否则，根据BIOS报告的扇区数量，使用/dev/PS)或/dev/at0
	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined		! 判断root_dev != 0
! 未定义根设备
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
! 如果不是以上两种磁盘，则死循环
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax		! 更新root_dev定义的根设备

! after that (everying loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:
! 然后，直接跳转到bootsect后面的setup程序进行运行。

	jmpi	0,SETUPSEG


! ------------------------- Main routine ends here -------------------------



! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
! 此程序加载系统到0x10000处，确保不会超过64KB的边界。我们尝试尽可能
! 快的加载它，如果可能的话加载整个磁道。
!
! sread 当前扇区已读扇区数
sread:	.word 1+SETUPLEN	! sectors read of current trace
head:	.word 0			! current head(当前磁头)
track:	.word 0			! current track(当前磁道)

read_it:
	mov ax,es			! 系统内存位置
	test ax,#0x0fff		! test按位与
die:	jne die			! es must be at 64kB boundary
	xor bx,bx			! bx is starting address within segment 初始化bx=0
! 重复读取
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read			! 若未读完，跳转到ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread	! ax = 扇区总数-已读扇区数 al为要读取的扇区数量
	mov cx,ax
	shl cx,#9		! shl 逻辑左移9位，cx *= 512
	add cx,bx		! cx += bx 计算本次读取的结束位置
	jnc ok2_read	! 如果cx > 0x10000，溢出CF=1，全部读完会超出本段的范围，需要按照0x10000-bx来计算读取扇区数量
	je ok2_read		! 如果cx == 0x10000，则按照当前的al扇区数，能够正好读完，不需要修改段地址es
! 修改段地址es
	xor ax,ax		
	sub ax,bx		! ax = 0x10000 - bx = 0 - bx
	shr ax,#9		! al = al / 512，得到要读取的扇区数
ok2_read:
	call read_track		! 读取整个磁道
	mov cx,ax			! al为已经读取的扇区数量
	add ax,sread		! ax = 以前读取的磁道数sread + 本次读取的磁道数al
	seg cs
	cmp ax,sectors		! 如果ax == sectors，则需要对sread减去sectors
	jne ok3_read
	mov ax,#1
	sub ax,head		! 更换磁头. 1-head(=0)=1; 1-head(=1)=0
	jne ok4_read
	inc track		! 当前head=1，将head更换为0的同时需要更换磁道
ok4_read:
	mov head,ax		! head = 1
	xor ax,ax		! 已读扇区数ax=0
! sread < sectors，不需要更换磁头和磁道
ok3_read:
	mov sread,ax	! sread = (sread == sectors) ? 0 : sread
	shl cx,#9
	add bx,cx		! bx += 本次已读扇区数*512
	jnc rp_read		! 如果bx到达0x10000，则会进位carry=1。
	mov ax,es
	add ax,#0x1000	！es += 0x10000, bx = 0;
	mov es,ax
	xor bx,bx
	jmp rp_read

! --------------------------------------------------
! 读取当前磁道
read_track:
	push ax			! 压栈，栈已经初始化过
	push bx
	push cx
	push dx

! BIOS中断调用0x13:低端磁盘服务
! AH=02h 读磁盘
! ES:BX 内存位置
! DH 驱动器号80h/81h;			DL 磁头号0~15
! CH 磁道号0-1023, 高两位为CL[8,9];	CL 扇区号1-17
! AL 读取的扇区数量1~80h
! 返回：AL 已经读区的扇区数,AH 0x00 Carry=1无错误
	mov dx,track
	mov cx,sread
	inc cx		! cx = sread + 1; 下一个要读取的扇区
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt	! 若读取磁道失败，则复位磁盘重试
	pop dx
	pop cx
	pop bx
	pop ax
	ret
! 读取磁道失败处理程序
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

! --------------------------------------------------
!
! This procedure turns off the floppy drive motor, so
! that we enter the kernel in a known state, and
! don't have to worry about it later.
!
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:

!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!
! setup.s从BIOS中读取系统信息并存放到系统内存中到合适位置。setup.s和system均
! 已经被bootsect.s程序加载到内存中。
!
! 此代码向bios查询内存/磁盘以及其它参数，并将其放置在一个“安全”到位置：0x90000-0x901FF
! (这是原来bootblock所在位置)。进入保护模式后，在此区域被缓冲块覆盖前，系统会去读取这些
! 参数。

! NOTE! These had better be the same as in bootsect.s!
! 注意！以下参数最好和bootsetc.s保持一致。

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.
! 系统加载过程运行成功，所以我们读取当前光标位置，并将其保存以备后用。

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.

! Get memory size (extended mem, kB)
! 读取扩展内存大小（kB）

	mov	ah,#0x88
	int	0x15
	mov	[2],ax

! Get video-card data:
! 读取显卡数据

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters
! 检查EGA/VGA和一些配置参数
	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data
! 读取硬盘hd0数据

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

! Get hd1 data
! 读取硬盘hd1数据
	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)
! 检查硬盘hd1是否存在
	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...
! 现在我们想要进入保护模式

	cli			! no interrupts allowed ! 关闭所有中断

! first we move the system to it's rightful place
! 首先将system移到正确的位置

	mov	ax,#0x0000	! 初始化
	cld			! 'direction'=0, movs moves forward 向前移动
do_move:
	mov	es,ax		! destination segment 目标段地址es=ax
	add	ax,#0x1000	! ax += 0x1000
	cmp	ax,#0x9000	! if (ax != 0x9000) 共移动0x80000个字节的内容
	jz	end_move
	mov	ds,ax		! source segment 源段地址ds=es+0x1000
	sub	di,di		! di = 0
	sub	si,si		! si = 0
	mov 	cx,#0x8000
	rep			! 每次移动0x10000个
	movsw
	jmp	do_move		! 循环执行do_move

! then we load the segment descriptors
! 然后载入段描述符

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0 载入idt(0,0)
! lgdt接一个6字节段段操作数来加载GDTR寄存器。头两个字节表示描述符长度，后4个
! 字节是描述符表的地址。（此处将GDTR看作一个数组指针，访问该数组需要知道数组地址和长度）
	lgdt	gdt_48		! load gdt with whatever appropriate 载入合适段gdt

! that was painless, now we enable A20
! 这些是简单的，现在我们使能A20
! 只有使能A20地址线，才能访问超过1MB的内存

	call	empty_8042
	mov	al,#0xD1		! command write 写命令
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on A20已使能
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.
! 好吧，我希望这成功了。现在我们必须重新对中断编程。我们将中断放到intel保留中断
! (位于0x20-0x2F)后面。这样就不会搞坏其它东西。可惜IBM在最初的PC中的确搞砸了，
! 并且他们后来没能纠正这个问题。以昵称BIOS将中断放在0x08-0x0F，这也是用来放置内部
! 硬件中断的地方。我们必须重新对8259进行编程，这很无趣。


	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! 好吧，的确没什么意思:-(。希望它能工作，我们不希望再使用BIOS（除了初始加载）。
! BIOS程序希望许多不必要的东西，尽管它不怎么有趣。这就是真正的码农干的活。
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
! 
! 现在，开始真正进入保护模式了。为了尽可能的简单，我们不实用寄存器设置或者其它，
! 我们使用gnu编译的32位程序来完成。我们只需要在32位保护模式下跳转到0x00000这一
! 绝对地址。
	mov	ax,#0x0001	! protected mode (PE) bit 保护模式位置位
	lmsw	ax		! This is it! 就是这样将cpu从实模式切换到保护模式，切换之后需接一条立即跳转指令清空指令序列
	jmpi	0,8		! jmp offset 0 of segment 8 (cs) 8为段选择符，取GDT表第1项到段基址0x0000:0x0000，开始执行system

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
! 此例程检查键盘命令序列为空，如果缓冲不为空，无法设置A20

empty_8042:
	.word	0x00eb,0x00eb   ! 延时，0x00eb为指令码
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret


; 全局描述符格式
; 
; 段基址[32:29];G;X;AVL;LIMIT[19:16]; P;DPL;1;TYPE;基地址[23:16]
; +--------+--------+--------+--------+
; |                 |                 |
; +--------+--------+--------+--------+
; |                 |                 |
; +--------+--------+--------+--------+
; 段基址[15:0];       段限长[15:0]
gdt:
; null段，防止非法访问
	.word	0,0,0,0		! dummy

; 代码段
	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb) 段限长根据粒度来确定
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386
; 数据段
	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L 

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries 2048字节/8(单个gdt描述符所占空间)=256个
	.word	512+gdt,0x9	! gdt base = 0X9xxxx 在setup所占的0x90200内存处定义gdt位置往后512个字节处的2KB为临时GDT表
	
.text
endtext:
.data
enddata:
.bss
endbss:

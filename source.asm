[org 0x0100]

start:
	call completeClrscr
	xor ax, ax
	mov es,ax
	mov ax, [es: 9*4]
	mov [oldkb], ax
	mov ax, [es: 9*4+2]
	mov [oldkb+2], ax

	mov ax, [es: 8*4]
	mov [oldtime], ax
	mov ax, [es: 8*4+2]
	mov [oldtime+2], ax

	cli
	mov word[es:9*4],kbsir
	mov word[es:9*4+2],cs
	mov word[es:8*4],timer
	mov word[es:8*4+2],cs
	sti

	call printbricks

infiniteloop: 
	jmp infiniteloop	

exitprogram:
	cmp word [noofbricks],0
	jne gamelost				;game won

	cmp word [timecount],120	;time exceeded
	jl wonwithintime
	
	call printgreenbox
	call printgamewon

	jmp unhooking

wonwithintime:
	shl word [score],1
	call printgreenbox
	call printgamewonwithtime
	
	jmp unhooking

gamelost:						;game lost
	call printredbox
	call gameover

unhooking:

	xor ax, ax
	mov es, ax

	mov ax, [oldkb] ; read old offset in ax
	mov bx, [oldkb+2] ; read old segment in bx
	cli ; disable interrupts
	mov [es:9*4], ax ; restore old offset from ax
	mov [es:9*4+2], bx ; restore old segment from bx

	mov ax, [oldtime] ; read old offset in ax
	mov bx, [oldtime+2] ; read old segment in bx
	mov [es:8*4], ax ; restore old offset from ax
	mov [es:8*4+2], bx ; restore old segment from bx

	sti

	;mov dx, start
	;add dx,15
	;mov cl,4
	;shr dx,cl
	;mov ax, 0x3100
	mov ax,0x4c00
	int 0x21

; FUNCTIONS

timer:
		 push ax
		 cmp word [pauseflag],1
		 je skipprintfireball
		 call printinfo

		 cmp word [noofbricks],0	;no bricks
		 jne skiplevel
		 call switchlevel

skiplevel:
		
		 cmp word [levelchangeflag],1			;level change printing
		 jne skipprintlevelchange
		 
		 inc word [levelchangecounter]

		 cmp word [levelchangecounter],72
		 jne skipprintlevelchange

		 mov word [levelchangecounter],0
		 mov word [levelchangeflag],0
		 call printemptylevelchange

skipprintlevelchange:

		 cmp byte [fireBall],1			;level change printing
		 jne skipprintfireball
		 
		 inc word [fireballcounter]

		 cmp word [fireballcounter],180
		 jne skipprintfireball

		 mov word [fireballcounter],0
		 call printemptyfireball
		
skipprintfireball:
		cmp word [pauseflag],1
		 je moooooovOn

		 cmp word [lives],0			;no lives
		 je exitprogram

		 cmp word [lives], 1
		 jne donotActive2Balls
		 cmp byte [once], 0
		 jne donotActive2Balls
		 mov word [ball1x], 40
		 mov word [ball1y], 20
	 	 mov byte [bf1], 1
	 	 mov word [movement1], 1
	 	 mov word [ball2x], 40
		 mov word [ball2y], 5
	 	 mov byte [bf2], 1
	 	 mov word [movement2], 1
	 	 mov byte [once], 1
	donotActive2Balls:

		mov word ax,[ballspeed]
		 cmp word [balltimer],ax
		 jl bypass

		 cmp word [FBcount], 180
		 jl moooooovOn
		 mov byte [fireBall], 0
		 mov byte [ballColor], 0x03
	moooooovOn:
		cmp word [pauseflag],1
		 je bypass
	 
		 cmp byte [bf1], 0
		 je GoToBall2


	PrintFirstBall:
		 push dx
		 mov dx, [ball1x]
		 mov word [ballx],dx 
		 mov dx, [ball1y]
		 mov word [bally],dx
		 mov word dx, [movement1]
		 mov word [movement], dx
		 mov byte dl, [bf1]
		 mov byte [bf], dl
		 pop dx

		 call printball
		 call updateballdirection

		 push dx
		 mov dx, [ballx]
		 mov word [ball1x],dx 
		 mov dx, [bally]
		 mov word [ball1y],dx
		 mov word dx, [movement]
		 mov word [movement1], dx
		 mov byte dl, [bf]
		 mov byte [bf1], dl
		 pop dx


		jmp ooperSeBypass
bypass:
	cmp word [pauseflag],1
	je ballendtimer
	jmp ballendtimer

ooperSeBypass:


GoToBall2:

		cmp byte [bf2], 0
		je doneWithBallPrinting

PrintBall2:

		 push dx
		 mov dx, [ball2x]
		 mov word [ballx],dx 
		 mov dx, [ball2y]
		 mov word [bally],dx
		 mov word dx, [movement2]
		 mov word [movement], dx
		 mov byte dl, [bf2]
		 mov byte [bf], dl
		 pop dx

		 call printball
		 call updateballdirection

		 push dx
		 mov dx, [ballx]
		 mov word [ball2x],dx 
		 mov dx, [bally]
		 mov word [ball2y],dx
		 mov word dx, [movement]
		 mov word [movement2], dx
		 mov byte dl, [bf]
		 mov byte [bf2], dl
		 pop dx

doneWithBallPrinting:
		 mov word [balltimer],0
		 jmp skipballtimer
ballendtimer:
		cmp word [pauseflag],1
		je exittimer
		 inc word [balltimer]
skipballtimer:
		 cmp word [counter], 18
		 jl endTimer

		 inc word [timecount]
		 mov word [counter],0

endTimer:
		cmp byte [fireBall], 0
		je skeeeeeep
			inc word [FBcount]
		skeeeeeep:

		 inc word [counter]
		 
		 mov al, 0x20
		 out 0x20, al    ; end of interrupt
		 pop ax
		 iret

balldisabled:
		 call gameover
exittimer:	
		 mov al, 0x20
		 out 0x20, al    ; end of interrupt
		 pop ax
		 iret

changebricksarray:
	push ax
	push di
	push bx

	mov di,0
	mov ax,30
cba1:
	mov bx,[bricks2 + di]
	mov [bricks + di],bx

	add di,2
	dec ax
	cmp ax,0
	jne cba1

	pop bx
	pop di
	pop ax

	ret

switchlevel:
	push ax
	cmp word [level],2
	je exitprogram
	
	mov word [level],2
	call changebricksarray
	mov word [noofbricks],12

	mov word [ballx],40				;update ball direction
	mov word [bally],20
	mov word [ball1x], 40
	mov word [ball1y], 20
	mov word [movement],1
	mov word [movement1],1
	mov word [movement2],1
	mov word [ballspeed],1
	

	call clrscr
	call printbricks
	call printlevelchange
	mov word [levelchangeflag],1
	mov word ax,[score]
	add word [score],ax

	pop ax
	ret

updateballdirection:
	cmp word [ballx],1		;left wall
	jne ubdcmp1

	cmp word [movement],6
	jne ubd1
	mov word [movement],2
	jmp updateballdexit

ubd1:
	mov word [movement],3

	jmp updateballdexit

ubdcmp1:
	cmp word [ballx],79	;right wall
	jne ubdcmp2

	cmp word [movement],2
	jne ubd2
	mov word [movement],6
	jmp updateballdexit

ubd2:
	mov word [movement],5

	jmp updateballdexit

ubdcmp2:
	cmp word [bally],0	;top 
	jne ubdcmp3

	cmp word [movement],1
	jne ubd3
	mov word [movement],4
	jmp updateballdexit

ubd3:
	cmp word [movement],2
	jne ubd4
	mov word [movement],3
	jmp updateballdexit

ubd4:
	mov word [movement],5
	jmp updateballdexit

ubdcmp3:
	cmp word [bally],21	;bottom
	jl updateballdexit
	call barballintersection


updateballdexit:
ret
	
barballintersection:
	push ax
	push bx

	mov ax,[bar]
	cmp ax, [ballx]
	jg lose

	add ax,12
	cmp ax,[ballx]
	jl lose


	sub ax, 4
	cmp ax, [ballx]
	jg barCenter 
	mov word [movement],2
	jmp intersectionexit

	barCenter:
	sub ax, 4
	cmp ax, [ballx]
	jg barLeft 
	mov word [movement],1
	jmp intersectionexit

	barLeft:
	mov word [movement],6
	jmp intersectionexit

ubd5:
	cmp word [movement],3
	jne ubd6
	mov word [movement],2
	jmp intersectionexit

ubd6:
	mov word [movement],6
	jmp intersectionexit

intersectionexit:
	pop bx
	pop ax
	ret

lose:
	cmp word [bally],23
	jne intersectionexit			;ball crossed

	cmp byte [bf1], 0
	jne checkOtherBall
	dec word [lives]
	
	mov ax,0xb800
	push ax
	pop es
	mov word [es:3880], 0x0720
	mov word [es:3882], 0x0720
	mov word [es:3884], 0x0720

	mov byte [bf1], 1

	call printemptybar
	mov word [bar],34
	call printbar

	jmp DoThePrintingNow
checkOtherBall:
	cmp byte [bf2], 0
	jne DoThePrintingNow
	dec word [lives]

	mov ax,0xb800
	push ax
	pop es
	mov word [es:3880], 0x0720
	mov word [es:3882], 0x0720
	mov word [es:3884], 0x0720

	mov byte [bf2], 1

	call printemptybar
	mov word [bar],34
	call printbar

DoThePrintingNow:
	call printemptyball

	mov word [ballx],40				;update ball direction
	mov word [bally],20
	mov word [movement],1
	mov byte [bf], 0
	
	pop bx
	pop ax
	ret

updateball:

	cmp word [movement],1
	jne nextcmp2

	dec word  [bally]
	jmp updateballexit

nextcmp2:
	cmp word [movement],2
	jne nextcmp3

	inc word  [ballx]
	dec word  [bally]
	jmp updateballexit

nextcmp3:
	cmp word [movement],3
	jne nextcmp4

	inc word [ballx]
	inc word [bally]
	jmp updateballexit

nextcmp4:
	cmp word  [movement],4
	jne nextcmp5

	inc word [bally],
	jmp updateballexit

nextcmp5:
	cmp word [movement],5
	jne nextcmp6

	dec word [ballx]
	inc word [bally]
	jmp updateballexit

nextcmp6:
	
	dec word [ballx]
	dec word [bally]

updateballexit:
	call checkAndBreakBrick
ret

printball:
		push ax
		push bx
		push cx
		push dx
		push es

		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0 ; normal attribute
		mov dl, [ballx] ; column
		mov dh, [bally]  ; row
		mov cx, 1   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, ball ; offset of string
		int 0x10   ; call BIOS video service

		call updateball

		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, [ballColor] ; normal attribute
		mov dl, [ballx] ; column
		mov dh, [bally]  ; row
		mov cx, 1   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, ball ; offset of string
		int 0x10   ; call BIOS video service
		pop es
		pop dx
		pop cx
		pop bx
		pop ax
		ret

printemptyball:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0 ; normal attribute
		mov dl, [ballx] ; column
		mov dh, [bally]  ; row
		mov cx, 1   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, ball ; offset of string
		int 0x10   ; call BIOS video service
		ret

printbricks: 
	call clrscr
	mov word [color], 0x11
	push word [temp]
	mov byte [temp], 0
	mov word [r], 7
	l2:
	mov word [inner],6
	l1:
		push ax
	    push bx
			mov word bx, [temp]
			add byte [temp], 2
			mov word ax, [cs:bricks+bx]
			mov byte [Brickrow], ah
			mov [t1],al
	   	pop bx
	   	pop ax

	   	cmp byte [t1], 0
	   	je skip_Printing

		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, [color] ; normal attribute
		mov dl, [t1] ; column
		mov dh, [Brickrow]  ; row
		mov cx, 12   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, message ; offset of string
		int 0x10   ; call BIOS video service
	skip_Printing:
	  	sub word [inner],1
	  	jnz l1
	  	add word [color], 0x11
	 	sub byte [r],2
	jg l2
	pop word [temp]
	call printbar
ret

printemptybar:
	mov word [color], 0x55
	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, 0 ; normal attribute
	mov dl, [bar]  ; column
	mov dh, 22  ; row
	mov cx, 13   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, message ; offset of string
	int 0x10   ; call BIOS video service
	ret

printbar:
	mov word [color], 0x77
	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, [color] ; normal attribute
	mov dl, [bar]  ; column
	mov dh, 22  ; row
	mov cx, 13   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, message ; offset of string
	int 0x10   ; call BIOS video service
ret

checkAndBreakBrick:
	push ax
	push bx
	push cx
	push word [temp]

	mov word [temp], 24
	mov bx, 0

	goThroughBricks:
	cmp word [bricks+bx], 0
	je idhrAaja

	mov ax, [bricks+bx]
	cmp ah, [bally]
	jne idhrAaja

	cmp al, [ballx]
	jg idhrAaja
	

	add al, 12
	cmp al, [ballx]
	jl idhrAaja

	jmp ooperSeAaja
idhrAaja:
	jmp skipBrick
ooperSeAaja:

	sub al, 12

	cmp byte [fireBall], 1
	je idhrAaja1

	cmp word [movement], 1
	jne nextBrickReflection
	mov word [movement], 4
	jmp updateBrick

nextBrickReflection:
	cmp word [movement], 4
	jne nextBrickReflection1
	mov word [movement], 1
	jmp updateBrick

nextBrickReflection1:
	cmp word [movement], 2
	jne nextBrickReflection2
	cmp al,[ballx]
	jne changeTo3
	mov word [movement], 6
	jmp updateBrick
changeTo3:
	mov word [movement], 3
	jmp updateBrick

jmp ooperSeAaja1
idhrAaja1:
	jmp updateBrick
ooperSeAaja1:

nextBrickReflection2:
	cmp word [movement], 3
	jne nextBrickReflection3
	cmp al,[ballx]
	jne changeTo2
	mov word [movement], 5
	jmp updateBrick
changeTo2:
	mov word [movement], 2
	jmp updateBrick

nextBrickReflection3:
	add al,12
	cmp word [movement], 6
	jne nextBrickReflection4
	cmp al,[ballx]
	jne changeTo5
	mov word [movement], 2
	jmp updateBrick
changeTo5:
	mov word [movement], 5
	jmp updateBrick

nextBrickReflection4:
	cmp word [movement], 5
	jne updateBrick
	cmp al,[ballx]
	jne changeTo6
	mov word [movement], 3
	jmp updateBrick
changeTo6:
	mov word [movement], 6

updateBrick:
	mov word [bricks+bx], 0
	add word [score],1
	dec word [noofbricks]
	
	cmp word [noofbricks], 16
	jne oyeHoye
	mov byte [fireBall], 1
	call printfireball
	mov byte [ballColor], 0x04
	oyeHoye:
	
	call printbricks
	call playsound

skipBrick:
	add bx, 2
	dec word [temp]
	cmp word [temp], 0
	jg goThroughBricks

	pop word [temp]
	pop cx
	pop bx
	pop ax
	;call clrscr
ret

kbsir:
	push ax

	in al, 0x60 ; read a char from keyboard port

	cmp al, 0x4B ; is the key left shift
	jne nextcmp ; no, try next comparison

	cmp word [bar],0
	je pauseoff1
	call printemptybar
	dec word [bar]
	call printbar

	cmp word [pauseflag],1
	jne pauseoff1
	call printemptypause
pauseoff1:
	mov word [pauseflag],0
	jmp nomatch 
nextcmp: 
	cmp al, 0x4D ; is the key left released
	jne nextcmp1 ; no, leave interrupt routine

	cmp word [bar],67
	je nomatch
	call printemptybar
	inc word [bar]
	call printbar

	cmp word [pauseflag],1
	jne pauseoff
	call printemptypause

pauseoff:

	mov word [pauseflag],0
	
	jmp nomatch

nextcmp1: 
	cmp al, 0x13 ; is the key r
	jne pauselabel ; no, leave interrupt routine

	call printemptyball

	mov word [ballx],20				;update ball direction
	mov word [bally],20
	mov word [ball1x], 20
	mov word [ball1y], 20

	mov word [movement],2
	mov word [movement1],2
	mov word [movement2],2
	
	jmp nomatch

pauselabel: 
	cmp al, 0x19 ; is the key p
	jne nomatch ; no, leave interrupt routine
	
	call printpause
	mov word [pauseflag],1

nomatch: 
	pop ax
	jmp far [cs:oldkb]

clrscr:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax    ; point es to video base
	xor di, di    ; point di to top left column
	mov ax, 0x0720   ; space char in normal attribute
	mov cx, 1000   ; number of screen locations
	cld     ; auto increment mode
	rep stosw    ; clear the whole screen
	pop di
	pop cx
	pop ax
	pop es
ret

completeClrscr:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax    ; point es to video base
	xor di, di    ; point di to top left column
	mov ax, 0x0720   ; space char in normal attribute
	mov cx, 2000   ; number of screen locations
	cld     ; auto increment mode
	rep stosw    ; clear the whole screen
	pop di
	pop cx
	pop ax
	pop es
ret

printinfo:
	mov word [color], 07
	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, [color] ; normal attribute
	mov dl, 1  ; column
	mov dh, 24  ; row
	mov cx, 7   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, scorestring ; offset of string
	int 0x10   ; call BIOS video service

	mov ax,3856
	push ax			;position to print
	push word [score]	;number to print
	call printnum

	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, [color] ; normal attribute
	mov dl, 13  ; column
	mov dh, 24  ; row
	mov cx, 7   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, livesstring ; offset of string
	int 0x10   ; call BIOS video service

	mov di,3880
	mov cx,[lives]
	mov ax,0xb800
	push ax
	pop es

continueprintinfo:
	mov word [es:di], 0x0403
	add di,2
	sub cx,1
	cmp cx,0
	jne continueprintinfo

	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, [color] ; normal attribute
	mov dl, 70  ; column
	mov dh, 24  ; row
	mov cx, 6   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, time ; offset of string
	int 0x10   ; call BIOS video service

	mov ax,3992
	push ax			;position to print
	push word [timecount]	;number to print
	call printnum

	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, [color] ; normal attribute
	mov dl, 26  ; column
	mov dh, 24  ; row
	mov cx, 7   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, levelstring ; offset of string
	int 0x10   ; call BIOS video service

	mov ax,3904
	push ax			;position to print
	push word [level]	;number to print
	call printnum
	ret

printnum: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
nextdigit: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, [bp+6] ; point di to top left column 
nextpos:
	pop dx ; remove a digit from the stack
	mov dh, 0x04 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
ret 4

gamelostroutine:
	call clrscr

printredbox:
	call completeClrscr
	push es
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax    ; point es to video base

	mov di,1600
	mov ax, 0xC420   ; space char in normal attribute
	mov cx, 80   ; number of screen locations
	cld     ; auto increment mode
	rep stosw    ; clear the whole screen

	mov di,2400
	mov ax, 0xC420   ; space char in normal attribute
	mov cx, 80   ; number of screen locations
	cld     ; auto increment mode
	rep stosw    ; clear the whole screen

	pop di
	pop cx
	pop ax
	pop es
	ret

printgreenbox:
	call completeClrscr
	push es
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax    ; point es to video base

	mov di,1600
	mov ax, 0xA220   ; space char in normal attribute
	mov cx, 80   ; number of screen locations
	cld     ; auto increment mode
	rep stosw    ; clear the whole screen

	mov di,2400
	mov ax, 0xA220   ; space char in normal attribute
	mov cx, 80   ; number of screen locations
	cld     ; auto increment mode
	rep stosw    ; clear the whole screen

	pop di
	pop cx
	pop ax
	pop es
	ret

printpoints:
	mov word [color], 07
	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, [color] ; normal attribute
	mov dl, 35  ; column
	mov dh, 13  ; row
	mov cx, 7   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, scorestring ; offset of string
	int 0x10   ; call BIOS video service

	mov ax,2164
	push ax			;position to print
	push word [score]	;number to print
	call printnum

	ret

printgamewon:
	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, 0x82 ; normal attribute
	mov dl, 32  ; column
	mov dh, 12  ; row
	mov cx, 18   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, gamewon ; offset of string
	int 0x10   ; call BIOS video service

	call printpoints
	ret

printgamewonwithtime:
	mov ah, 0x13  ; service 13 - print string
	mov al, 1   ; subservice 01 – update cursor
	mov bh, 0   ; output on page 0
	mov bl, 0x82 ; normal attribute
	mov dl, 10  ; column
	mov dh, 12  ; row
	mov cx, 58   ; length of string
	push cs
	pop es    ; segment of string
	mov bp, gamewonintime ; offset of string
	int 0x10   ; call BIOS video service
	
	call printpoints
	ret

gameover:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0x84 ; normal attribute
		mov dl, 26 ; column
		mov dh, 12 ; row
		mov cx, 26   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, gameoverstring ; offset of string
		int 0x10   ; call BIOS video service

		call printpoints
		ret

printlevelchange:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0x82 ; normal attribute
		mov dl, 35 ; column
		mov dh, 24 ; row
		mov cx, 30   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, levelchangestring ; offset of string
		int 0x10   ; call BIOS video service
		ret

printemptylevelchange:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0x00 ; normal attribute
		mov dl, 35 ; column
		mov dh, 24 ; row
		mov cx, 30   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, levelchangestring ; offset of string
		int 0x10   ; call BIOS video service
		ret

printfireball:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0x82 ; normal attribute
		mov dl, 35 ; column
		mov dh, 24 ; row
		mov cx, 23   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, fireballstring ; offset of string
		int 0x10   ; call BIOS video service
		ret

printemptyfireball:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0 ; normal attribute
		mov dl, 35 ; column
		mov dh, 24 ; row
		mov cx, 23   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, fireballstring ; offset of string
		int 0x10   ; call BIOS video service
		ret

printpause:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0x82 ; normal attribute
		mov dl, 35 ; column
		mov dh, 0 ; row
		mov cx, 14   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, pausestring ; offset of string
		int 0x10   ; call BIOS video service
		ret

printemptypause:
		mov ah, 0x13  ; service 13 - print string
		mov al, 1   ; subservice 01 – update cursor
		mov bh, 0   ; output on page 0
		mov bl, 0 ; normal attribute
		mov dl, 35 ; column
		mov dh, 0 ; row
		mov cx, 14   ; length of string
		push cs
		pop es    ; segment of string
		mov bp, pausestring ; offset of string
		int 0x10   ; call BIOS video service
		ret

playsound:
mov     al, 182         
        out     43h, al        
        mov     ax, 2031        
                               
        out     42h, al      
        mov     al, ah      
        out     42h, al 
        in      al, 61h        
                                
        or      al, 00000011b   
        out     61h, al         
        mov     bx, 10          
pause1:
        mov     cx, 5000
pause2:
        dec     cx
        jne     pause2
        dec     bx
        jne     pause1
        in      al, 61h        
                               
        and     al, 11111100b   
        out     61h, al        
		ret

;Variables
message dw '_____________'
bricks dw 0x0101, 0x010E, 0x011B, 0x0129, 0x0136, 0x0143, 0x0301, 0x030E, 0x031B, 0x0329, 0x0336, 0x0343, 0x0501, 0x050E, 0x051B, 0x0529, 0x0536, 0x0543, 0x0701, 0x070E, 0x071B, 0x0729, 0x0736, 0x0743
bricks2 dw 0x0101, 0, 0x011B, 0, 0x0136, 0, 0, 0x030E, 0, 0x0329, 0, 0x0343, 0x0501, 0, 0x051B, 0, 0x0536, 0, 0, 0x070E, 0, 0x0729, 0, 0x0743
t1 db 0
r db 0
Brickrow db 0
temp dw 0
inner dw 0
color dw 0x11
bar dw 34
counter dw 0
scorestring db 'Score: '
livesstring db 'Lives: '
score dw 0
lives dw 3
time db 'Time: '
timecount dw 0
oldkb: dd 0 
oldtime: dd 0
ballx dw 0
bally dw 0
bf db 0

ball1x dw 40
ball1y dw 20
bf1 db 1

ball2x dw 40
ball2y dw 20
bf2 db 0
ballspeed dw 1

movement dw 1
movement1 dw 1
movement2 dw 1

ball db 0x4
ballempty db ' '
balltimer dw 0
disableball dw 0
gameoverstring db 'Oops! You lost ! LIVES : 0'
gamewonintime db 'You Won the game within 2 minutes. You get DOUBLE points!!'
gamewon db 'You Won the game !'
noofbricks dw 24
totalbricks dw 24
fireBall db 0
FBcount dw 0
once db 0
ballColor db 0x03
level dw 1
levelstring db 'Level :'
levelchangestring db 'YOU MOVED TO THE NEXT LEVEL !!!'
levelchangecounter dw 0
levelchangeflag dw 0
fireballstring db 'FIRE BALL ACTIVATED !!!'
fireballcounter dw 0
pauseflag dw 0
pausestring db 'GAME IS PAUSED'
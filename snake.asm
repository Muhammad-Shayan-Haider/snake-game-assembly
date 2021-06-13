org 100h
	jmp start

speed: dw 100
snake: dw '0'
mouth: dw '(' 
s: dw 20
co: dw 0x0201 
times 239 dw 0
flag: dw 0, 0	;up 1 down 2 right 3 left 4 next word for check i.e if down no up and VV
lives: dw 3
millisec: dw 0
sec: dw 0
minutes: dw 0
space: dw 0111000000000000b
len: dw 78
wid: dw 20
sec_flag: dw 0
sp_space: dw 0111000000000000b
fruit: dw 0
life: db 'Lives'
score: db 'Score'
sc: dw 0
won: db 'You won'
over: db '---Game over---_-Your score-'
lev: db 'level'
levels: dw 0
fr: db 222
T_life: db 'Total_lives'
sound:
	mov al, 182         ; Prepare the speaker for the
        out 43h, al         ;  note.
        mov ax, 6087        ; Frequency number (in decimal)
                                ;  for middle C.
        out 42h, al         ; Output low byte.
        mov al, ah          ; Output high byte.
        out 42h, al 
        in  al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or  al, 00000011b   ; Set bits 1 and 0.
        out 61h, al         ; Send new value.
        mov bx, 0          ; Pause for duration of note.
pause1:
        mov cx, 1
pause2:
        dec cx
        jne pause2
        dec bx
        jne pause1
        in  al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and al, 11111100b   ; Reset bits 1 and 0.
        out 61h, al
	ret
sound1:
	
	mov al, 182         ; Prepare the speaker for the
        out 43h, al         ;  note.
        mov ax, 8126        ; Frequency number (in decimal)
                                ;  for middle C.
        out 42h, al         ; Output low byte.
        mov al, ah          ; Output high byte.
        out 42h, al 
        in  al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or  al, 00000011b   ; Set bits 1 and 0.
        out 61h, al         ; Send new value.
        mov bx, 0          ; Pause for duration of note.
pause3:
        mov cx, 1
pause4:
        dec cx
        jne pause4
        dec bx
        jne pause3
        in  al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and al, 11111100b   ; Reset bits 1 and 0.
        out 61h, al
	ret
sound2:
	
	mov al, 182         ; Prepare the speaker for the
        out 43h, al         ;  note.
        mov ax, 1207        ; Frequency number (in decimal)
                                ;  for middle C.
        out 42h, al         ; Output low byte.
        mov al, ah          ; Output high byte.
        out 42h, al 
        in  al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or  al, 00000011b   ; Set bits 1 and 0.
        out 61h, al         ; Send new value.
        mov bx, 0          ; Pause for duration of note.
pause5:
        mov cx, 1
pause6:
        dec cx
        jne pause6
        dec bx
        jne pause5
        in  al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and al, 11111100b   ; Reset bits 1 and 0.
        out 61h, al
	ret
sound3:
	
	mov al, 182         ; Prepare the speaker for the
        out 43h, al         ;  note.
        mov ax, 1140        ; Frequency number (in decimal)
                                ;  for middle C.
        out 42h, al         ; Output low byte.
        mov al, ah          ; Output high byte.
        out 42h, al 
        in  al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or  al, 00000011b   ; Set bits 1 and 0.
        out 61h, al         ; Send new value.
        mov bx, 0          ; Pause for duration of note.
pause7:
        mov cx, 1
pause8:
        dec cx
        jne pause8
        dec bx
        jne pause7
        in  al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and al, 11111100b   ; Reset bits 1 and 0.
        out 61h, al
	ret
sound4:
	
	mov al, 182         ; Prepare the speaker for the
        out 43h, al         ;  note.
        mov ax, 3834        ; Frequency number (in decimal)
                                ;  for middle C.
        out 42h, al         ; Output low byte.
        mov al, ah          ; Output high byte.
        out 42h, al 
        in  al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or  al, 00000011b   ; Set bits 1 and 0.
        out 61h, al         ; Send new value.
        mov bx, 0          ; Pause for duration of note.
pause9:
        mov cx, 1
pause10:
        dec cx
        jne pause10
        dec bx
        jne pause9
        in  al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and al, 11111100b   ; Reset bits 1 and 0.
        out 61h, al
	ret

beeps:
	call sound
	call sound1
	ret
dis_lives:
	push ax
	push bx
	push cx
	push di
	push es
	
	mov di, 0
	mov ax, 0xb800
	mov es, ax
	mov cx, 5
	mov ah, 7
	mov bx, 0
l10:
	mov al, byte[life+bx]
	add bx, 1
	mov word[es:di], ax
	add di, 2
	loop l10
	add di, 4
	mov ax, word[lives]
	add ax, 48
	mov ah, 7
	mov word[es:di], ax
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret
dis_win:
	push ax
	push bx
	push cx
	push di
	push es
	
	mov di, 1500
	mov ax, 0xb800
	mov es, ax
	mov cx, 7
	mov ah, 7
	mov bx, 0
l12:
	mov al, byte[won+bx]
	add bx, 1
	mov word[es:di], ax
	add di, 2
	loop l12
	
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret
dis_level:
	push ax
	push bx
	push cx
	push di
	push es
	
	mov di, 3958
	mov ax, 0xb800
	mov es, ax
	mov cx, 5
	mov ah, 7
	mov bx, 0
l15:
	mov al, byte[lev+bx]
	add bx, 1
	mov word[es:di], ax
	add di, 2
	loop l15
	add di, 4
	mov ax, word[levels]
	add ax, 48
	mov ah, 7
	mov word[es:di], ax
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret

dis_over:
	push ax
	push bx
	push cx
	push di
	push es
	
	mov di, 1824
	mov ax, 0xb800
	mov es, ax
	mov cx, 28
	mov ah, 7
	mov bx, 0
l13:
	mov al, byte[over+bx]
	add bx, 1
	mov word[es:di], ax
	add di, 2
	loop l13
	add di, 2
	call printscore
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret

dis_score:
	push ax
	push bx
	push cx
	push di
	push es
	
	mov di, 60
	mov ax, 0xb800
	mov es, ax
	mov cx, 5
	mov ah, 7
	mov bx, 0
l11:
	mov al, byte[score+bx]
	add bx, 1
	mov word[es:di], ax
	add di, 2
	loop l11
	add di, 4
	call printscore
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret
dis_T_lives:
	push ax
	push bx
	push cx
	push di
	push es
	
	mov di, 3908
	mov ax, 0xb800
	mov es, ax
	mov cx, 11
	mov ah, 7
	mov bx, 0
l16:
	mov al, byte[T_life+bx]
	add bx, 1
	mov word[es:di], ax
	add di, 2
	loop l16
	add di, 4
	mov ax, 3
	add ax, 48
	mov ah, 7
	mov word[es:di], ax
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret
printscore:
	mov ax, 0xb800
	mov es, ax 
	mov ax, word[sc]
	mov bx, 10
	mov cx, 0 
next_digit2: 
	mov dx, 0 
	div bx 
	add dl, 0x30
	push dx
	inc cx 
	cmp ax, 0 
	jnz next_digit2
next_pos2: 
	pop dx
	mov dh, 0x07
	mov [es:di], dx
	add di, 2 
	loop next_pos2 
	
	ret 
printchar:
	push bp 
	mov bp, sp
	push ax
	push bx
	push es
	push di
	push cx
	mov ax, 0xb800
	mov es, ax
	
	mov ax, word[bp+6] ;co_ord
	mov bx, word[bp+4] ;mouth
	mov cx, ax
	mov al, 80
	mul ch
	add al, cl
	shl ax, 1
	mov di, ax
	
	mov word[es:di], bx
	pop cx
	pop di
	pop es
	pop bx
	pop ax
	pop bp
	ret 4

begin:
	call sound4
	dec word[lives]
	push bx
	push ax
	mov bx, word[s]
	shl bx, 1
label1:
	push word[co+bx]
	push word[space]
	call printchar
	mov ax, word[space]
	mov word[co+bx], ax
	sub bx, 2
	cmp bx, 0
	ja label1
	call special_clr
	mov word[s], 20
	pop ax
	pop bx
	ret
BEGIN:
	call begin
	mov ax, 0x0201
	mov word[flag], 3
	mov word[flag+2], 3
	ret
UP:
	cmp word[flag+2], 2
	je f1
	mov word[flag], 1
	mov word[flag+2], 1
f1:
	jmp eoi1
up_move:
	sub ah, 1
	cmp ah, 1
	jne fin1
	call BEGIN
fin1:
	jmp return1

DOWN:
	cmp word[flag+2], 1
	je f2
	mov word[flag], 2
	mov word[flag+2], 2
f2:
	jmp eoi1
down_move:
	add ah, 1
	cmp ah, 22
	jne fin2
	call BEGIN
fin2:
	jmp return1

RIGHT:
	cmp word[flag+2], 4
	je f3
	mov word[flag], 3
	mov word[flag+2], 3
f3:
	jmp eoi1
right_move:
	add al, 1
	cmp al, 79
	jne fin3
	call BEGIN
fin3:
	jmp return1
	
LEFT:
	cmp word[flag+2], 3
	je f4
	mov word[flag], 4
	mov word[flag+2], 4
f4:
	jmp eoi1
left_move:
	sub al, 1
	cmp al, 0
	jne fin4
	call BEGIN
fin4:
	jmp return1

clrscreen:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax
	xor di, di
	mov ax, word[space]
	mov cx, 2000
	cld
	rep stosw
	pop di
	pop cx
	pop ax
	pop es
	ret
special_clr:
	push es
	push ax
	push cx
	push di
	push si
	mov si, 0
	mov ax, 0xb800
	mov es, ax
	mov di, 322
l1:
	mov cx, 78
	mov ax, word[sp_space]
	cld
	rep stosw
	add si, 1
	add di, 4
	cmp si, 20
	
	jb l1
	pop si
	pop di
	pop cx
	pop ax
	pop es
	ret
rectangle:
	push bp
	mov bp, sp
	push es
	push ax
	push di
	push bx
	inc word[lives]
	mov di, word[bp+8]
	add word[bp+6], di
	mov ax, 0xb800
	mov es, ax
	mov ah, 01100000b
	mov al, '+'
	mov bx, 0
firstline:
	mov word[es:di], ax
	add di, 2
	cmp di, word[bp+6]
	jb firstline
	add di, 160
secondline:
	mov word[es:di], ax
	add di, 160
	add bx, 2
	cmp bx, word[bp+4]
	jb secondline
	mov bx, word[bp+8]
	sub word[bp+6], bx
	mov bx, 0
	sub di, 2
thirdline:
	mov word[es:di], ax
	sub di, 2
	add bx, 2
	cmp bx, word[bp+6]
	jne thirdline
	sub di, 160
	mov bx, 0
fourthline:
	mov word[es:di], ax
	sub di, 160
	add bx, 2
	cmp bx, word[bp+4]
	jb fourthline
	
fin:
	pop bx
	pop di
	pop ax
	pop es
	pop bp
	ret 6
setmin:
	mov word[es:di], 0x0730
	add di, 2
	push ax
	add ax, 0x30
	mov ah, 7
	mov word[es:di], ax
	pop ax
	jmp min_set
printsec: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax 
	mov di, 140
	mov ax, [bp+4]
	cmp ax, 10
	jb setmin
	mov bx, 10
	mov cx, 0 
nextdigit1: 
	mov dx, 0 
	div bx 
	add dl, 0x30
	push dx
	inc cx 
	cmp ax, 0 
	jnz nextdigit1
	mov di, 140 
nextpos1: 
	pop dx
	mov dh, 0x07
	mov [es:di], dx
	add di, 2 
	loop nextpos1
min_set:
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
printmin: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax 
	mov ax, [bp+4]
	mov bx, 10
	mov cx, 0 
next_digit: 
	mov dx, 0 
	div bx 
	add dl, 0x30
	push dx
	inc cx 
	cmp ax, 0 
	jnz next_digit
	mov di, 136 
next_pos: 
	pop dx
	mov dh, 0x07
	mov [es:di], dx
	add di, 2 
	loop next_pos 
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
printformat:
	push di
	push es
	push ax
	
	mov ax, 0xb800
	mov es, ax
	mov di, 122
	mov ah, byte[space+1]
	mov al, 'T'
	mov word[es:di], ax
	add di, 2
	mov al, 'i'
	mov word[es:di], ax
	add di, 2
	mov al, 'm'
	mov word[es:di], ax
	add di, 2
	mov al, 'e'
	mov word[es:di], ax
	add di, 2
	add di, 2
	add di, 2
	mov ax, 0x0730
	mov word[es:di], ax
	add di, 2
	mov word[es:di], ax
	mov ah, 7
	mov al, ':'
	add di, 2
	mov word[es:di], ax
	add di, 2
	mov ax, 0x0730
	mov word[es:di], ax
	add di, 2
	mov word[es:di], ax
	pop ax
	pop es
	pop di
	ret
setspeed:
	
	add word[levels], 1
	shl word[speed], 1
	add byte[sp_space+1], 2
	
	jmp exit_time
	
timer:
	push ax
        add word[cs:millisec], 1
        cmp word[cs:millisec], 19
        jb eoi
	add word[sec], 1
        mov word[cs:millisec], 0
	cmp word[sec], 60
	jb eoi
	mov word[sec], 0
	inc word[minutes]
eoi:
	
	call printformat
	push word[minutes]
	call printmin
	push word[sec]
	call printsec
	
	mov ax, word[sec]
	mov bl, 20
	div bl
	cmp ah, 0
	je setspeed
	cmp word[lives], 0
	je endgame
	cmp word[minutes], 4
	je endgame1
exit_time:
        
	call display_fr
	
	call dis_score
	
	mov al, 0x20
        out 0x20, al
	pop ax
        iret
endgame1:
	cmp word[lives], 0
	je endgame
	dec word[lives]
	mov word[minutes], 0
	mov word[sec], 0
	jmp exit_time
endgame:
	pop ax
	pop ax
	pop ax
	pop ax
	jmp final
kbisr:
	push ax
	in al, 0x60
	cmp al, 'H'
	je UP
	cmp al, 'P'
	je DOWN
	cmp al, 'M'
	je RIGHT
	cmp al, 'K'
	je LEFT
eoi1:
	mov al, 0x20
	out 0x20, al
	pop ax
	iret
print_snake:
	call special_clr
	call display_fr
	
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	mov di, 1
	mov si, 2
	push cs
	pop es
	mov ah, 0x13
	mov bh, 0
	mov bl, 0x20
	mov dx, word[co]
	mov bp, mouth
	mov cx, 1
	int 10h
print:
	push cs
	pop es
	mov ah, 0x13
	mov bh, 0
	mov bl, 0x20
	mov dx, word[co+si]
	mov bp, snake
	mov cx, 1
	int 10h
	add si, 2
	add di, 1
	cmp di, word[s]
	jb print
	
	push cs
	pop es
	mov ah, 0x13
	mov bh, 0
	mov bl, byte[sp_space+1]
	mov dx, word[co+si]
	mov bp, space
	mov cx, 1
	int 10h
	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret	
Begin:
	call begin
	mov word[co], 0x0201
	mov word[flag], 3
	mov word[flag+2], 3
	jmp l3
	
update_co_ord:
	mov ah, 01100000b
	mov al, '+'
	push 0x1100
	push ax
	call printchar
	call sound1
        push bx
        push ax
        mov bx, word[s]
        shl bx, 1
	push bx
	mov ax, word[co]
l2:
	cmp word[co+bx],  ax
	je Begin
	sub bx, 2
	cmp bx, 4
	ja l2
l3:
	pop bx
loop1:
        mov ax, word[co+bx-2]
        mov word[co+bx], ax
        sub bx, 2
        cmp bx, 0
        ja loop1

        mov ax, word[co]	;update_co_ord mai baar baar update kre ga
        cmp word[flag], 1
	je up_move
	cmp word[flag], 2
	je down_move
	cmp word[flag], 3
	je right_move
	cmp word[flag], 4
	je left_move
return1:
        mov word[co], ax
	;mov ah, 0x20
	mov al, '('
	;mov word[snake], 0x2030
	;mov word[mouth], ax
	call print_snake
	
	pop ax
        pop bx
        ret
b_row:
	mov ah, 5
	jmp jmp_back
a_row:
	mov ah, 4
	jmp jmp_back
b_col:
	mov al, 4
	jmp jmp_back
a_col:
	mov al, 60
	jmp jmp_back
set_co:
	shl al, 1
	shr ah, 2
	jmp jmp_back
	
set_fruit:
	push ax
	push bx
	push dx
	push di
	mov ax, word[sec]
	add ah, 1
	mov bl, 160
	mul bl
	add ax, word[millisec]
	;0x0201
	cmp al, 3
	jb b_col
	cmp al, 78
	ja a_col
jmp_back:
	cmp ah, 3
	jb b_row
	cmp ah, 20
	ja a_row
	
jmp_back1:
	mov bx, word[s]
	shl bx, 1
l6:
	cmp ax, word[co+bx]
	je set_co
	sub bx, 2
	cmp bx, 0
	ja l6
	cmp ax, word[co]
	je set_co
ret1:	
	mov word[fruit], ax
	pop di
	pop dx
	pop bx
	pop ax
	ret
display_fr:
	push ax
	mov ah, byte[space+1]
	mov al, byte[fr]
	push word[fruit]
	push ax
	call printchar
	pop ax
	ret
set_f_wrapp:
	jmp set_f
check_again:
	cmp word[flag], 3
	je set_f_wrapp
	jmp come_back

check_again1:
	cmp word[flag], 4
	je set_f_wrapp
	jmp come_back
check_again2:
	cmp word[flag], 2
	je set_f
	jmp come_back
check_head:
	mov ah, 0x20
	mov al, '('
	mov bh, 0x20
	mov bl, '0'
	sub di, 2
	cmp word[es:di], ax
	je check_again
	add di, 2
	add di, 2
	cmp word[es:di], ax
	je check_again1
	sub di, 2
	add di, 160
	cmp word[es:di], ax
	je check_again2
	sub di, 160
	sub di, 160
	cmp word[es:di], ax
	je check_again3
	mov ax, word[co]
	cmp word[fruit], ax
	je set_f
	jmp come_back
check_again3:
	cmp word[flag], 1
	je set_f
	jmp come_back
check_fruit:
	push es
	push ax
	push di
	push cx
	push bx
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	mov cx, 2000
	mov ah, byte[space+1]
	mov al, byte[fr]
loop2:
	cmp word[es:di], ax
	je check_head
	add di, 2
	loop loop2
come_back:	
	pop bx
	pop cx
	pop di
	pop ax
	pop es
	ret
set_f:
	call set_fruit
	add word[sc], 1
	call special_clr
	add word[s], 4
	add byte[fr], 1
	call sound3
	jmp come_back
win:
	call dis_win
	jmp final1
start:
	call clrscreen 
	mov ax, 162
	push ax
	shl word[len], 1
	push word[len]
	shl word[wid], 1
	push word[wid]
	call rectangle
	call dis_T_lives
	mov ax, 0
	mov es, ax
	cli
	mov word[es:8h*4], timer
	mov word[es:8h*4+2], cs
	mov word[es:9h*4], kbisr
	mov word[es:9h*4+2], cs
	sti
	call set_fruit
	
lab1:
	call check_fruit
	call update_co_ord
	call dis_lives
	;call dis_level
delay1:
	mov cx, word[speed]
delay:
	;mov ax, word[co]	
	add cx, 3
	cmp cx, 0xffff
	jb delay
	jmp lab1
final:
	cmp word[s], 240
	jae win
final1:
	call dis_over
	call sound2
	mov ax, 4ch
	int 21h
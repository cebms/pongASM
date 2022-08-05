org 0x7e00
jmp main

playerPositionY dw 0
playerPositionX dw 0
secPlayerPositionY dw 0
secPlayerPositionX dw 0
movCount dw 0
movCountSec dw 0
racketSpeed dw 7

ballPositionX dw 100
ballPositionY dw 60

ballDirectionX dw 0; 0 esquerda, 1 parado, 2 direita1
ballDirectionY dw 1; 0 baixo, 1 parado, 2 direita


main:
    xor ax, ax
    mov ds, ax
    mov cx, ax
    mov dx, ax

    ;Setup ISR for the scancode
    call init
    ;Clear screen
    mov ax, 03h
    int 10h

    mov word[playerPositionY], 75
    mov word[playerPositionX], 10

    mov word[secPlayerPositionY], 75
    mov word[secPlayerPositionX], 303

    call setVideoMode

    mainLoop:

        call drawRacket
        
        call drawRacketSec
        
        call drawBall

        call movBall
        
        mov al, 1fh           ;s
        call is_scancode_pressed
        ; jz mainLoops
        jnz delayFirstPlayerDown

        checkUp:
        mov al, 11h           ;w 
        call is_scancode_pressed
        jnz delayFirstPlayerUp

        checkDownSec:
        mov al, 50h         
        call is_scancode_pressed
        jnz delaySecPlayerDown

        checkUpSec:
        mov al, 48h
        call is_scancode_pressed
        jnz delaySecPlayerUp

        jmp mainLoop   
       

jmp done

;----- GAME FUNCTIONS -----

movBall:
    controlY:
        cmp word[ballPositionY], 5
        je goDown
        cmp word[ballPositionY], 190
        je goUp

    controlX:
        ;se estiver perto de bater na raquete da esquerda
        cmp word[ballPositionX], 25
        je checkCol
        ;se estiver perto de bater na raquete da direita
        cmp word[ballPositionX], 290
        je checkColSec

    movX:
        cmp word[ballDirectionX], 1
        ja incBallX
        jb decBallX

    movY:
        cmp word[ballDirectionY], 1
        jb incBallY
        ja decBallY

    endMov:
ret

checkCol:
    ;preciso checar se ele esta na mesma altura
    pusha
    checkBaixo:
        mov ax, word[ballPositionY]
        mov bx, word[playerPositionY]
        add bx, 32
        cmp ax, bx
        ja movX  ; se bx+32 < ax nao colidiu
    checaCima:
        mov ax, word[ballPositionY]
        add ax, 8
        mov bx, word[playerPositionY]
        cmp bx, bx
        jb movX
    popa

jmp goRight

checkColSec:
    ;preciso checar se ele esta na mesma altura
    pusha
    ; checkBaixo1
        mov ax, word[ballPositionY]
        mov bx, word[secPlayerPositionY]
        add bx, 32
        cmp ax, bx
        ja movX  ; se bx+32 < ax nao colidiu
    ; checaCima:
        mov ax, word[ballPositionY]
        add ax, 8
        mov bx, word[secPlayerPositionY]
        cmp bx, bx
        jb movX
    popa

jmp goLeft

goRight:
    mov word[ballDirectionX], 2
jmp movX
goLeft:
    mov word[ballDirectionX], 0
jmp movX
goDown:
    mov word[ballDirectionY], 0
jmp controlX
goUp:
    mov word[ballDirectionY], 2
jmp controlX


incBallX:
    inc word[ballPositionX]
jmp movY
decBallX:
    dec word[ballPositionX]
jmp movY
incBallY:
    inc word[ballPositionY]
jmp endMov
decBallY:
    dec word[ballPositionY]
jmp endMov

drawBall:
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, word[ballPositionY]

    for5:
        mov bx, 8 ;altura da bola 
        add bx, word[ballPositionY]
        cmp dx, bx  ; fim da bola (linhas)
        je .fim5
        mov cx, word[ballPositionX]  ; inicio da bola (coluna)
        jmp .for6
        jmp for5

        .fim5:
        ret

        ; laco for para varrer as 8 colunas da imagem
        .for6:
        mov bx, 8 ;largura da bola
        add bx, word[ballPositionX]
        cmp cx, bx   ; direita da bola
        je .fim6
        mov al, 0x0f
        mov ah, 0ch
        int 10h
        inc cx
        jmp .for6

        .fim6:
        inc dx
        jmp for5

ret

delayFirstPlayerDown:
inc word[movCount]
mov ax, word[racketSpeed]
cmp word[movCount], ax
je moveDown
jmp checkUp

delaySecPlayerDown:
inc word[movCountSec]
mov ax, word[racketSpeed]
cmp word[movCountSec], ax
je moveDownSec
jmp checkUpSec

delayFirstPlayerUp:
inc word[movCount]
mov ax, word[racketSpeed]
cmp word[movCount], ax
je moveUp
jmp checkDownSec

delaySecPlayerUp:
inc word[movCountSec]
mov ax, word[racketSpeed]
cmp word[movCountSec], ax
je moveUpSec
jmp mainLoop

setVideoMode: 
    mov ah, 0       ; primeiro parametro para chamar modo de video
    mov al, 13h     ; segundo parametro para chamar modo de video
    int 10h
ret

drawRacket:
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, word[playerPositionY]

    for1:
        mov bx, 32 ;altura da raquete 
        add bx, word[playerPositionY]
        cmp dx, bx  ; fim da raquete (linhas)
        je .fim1
        mov cx, word[playerPositionX]  ; inicio da raquete (coluna)
        jmp .for2
        jmp for1

        .fim1:
        ret

        ; laco for para varrer as 8 colunas da imagem
        .for2:
        mov bx, 8 ;largura da raquete
        add bx, word[playerPositionX]
        cmp cx, bx   ; direita da raquete
        je .fim2
        mov al, 0x0f
        mov ah, 0ch
        int 10h
        inc cx
        jmp .for2

        .fim2:
        inc dx
        jmp for1
ret

drawRacketSec:
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, word[secPlayerPositionY]

    lop1:
        mov bx, 32 ;altura da raquete 
        add bx, word[secPlayerPositionY]
        cmp dx, bx  ; fim da raquete (linhas)
        je .end1
        mov cx, word[secPlayerPositionX]  ; inicio da raquete (coluna)
        jmp .lop2
        jmp lop1

        .end1:
        ret

        ; laco for para varrer as 8 colunas da imagem
        .lop2:
        mov bx, 8 ;largura da raquete
        add bx, word[secPlayerPositionX]
        cmp cx, bx   ; direita da raquete
        je .end2
        mov al, 0x0f
        mov ah, 0ch
        int 10h
        inc cx
        jmp .lop2

        .end2:
        inc dx
        jmp lop1
ret

moveDown:
    mov word[movCount], 0
    cmp word[playerPositionY], 168
    je checkUp
    mov dx, word[playerPositionY]
    mov cx, word[playerPositionX]
    mov bx, 0x00
    clearTraceDown:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceDown

    mov ax, word[playerPositionY]
    inc ax 
    mov [playerPositionY], ax
    mov ah, 0x04
    int 16h
    call drawRacket
jmp checkUp

done:
    jmp $

moveUp:
    mov word[movCount], 0
    cmp word[playerPositionY], 0
    je checkDownSec
    mov dx, word[playerPositionY]
    mov cx, word[playerPositionX]
    mov bx, 0x00
    add dx, 31
    clearTraceUp:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceUp

    mov ax, word[playerPositionY]

    dec ax
    mov [playerPositionY], ax
    mov ah, 0x04
    int 16h
    call drawRacket
jmp checkDownSec

moveUpSec:
    mov word[movCountSec], 0
    cmp word[secPlayerPositionY], 0
    je mainLoop
    mov dx, word[secPlayerPositionY]
    mov cx, word[secPlayerPositionX]
    mov bx, 0x00
    add dx, 31
    clearTraceUpSec:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceUpSec

    mov ax, word[secPlayerPositionY]

    dec ax
    mov [secPlayerPositionY], ax
    mov ah, 0x04
    int 16h
jmp mainLoop

moveDownSec:
    mov word[movCountSec], 0
    cmp word[secPlayerPositionY], 168
    je checkUpSec
    mov dx, word[secPlayerPositionY]
    mov cx, word[secPlayerPositionX]
    mov bx, 0x00
    clearTraceDownSec:
        mov al, 0x00 ; black pixel
        mov ah, 0ch
        int 10h
        inc cx
        inc bx
        cmp bx, 8
    jne clearTraceDownSec

    mov ax, word[secPlayerPositionY]

    inc ax
    mov [secPlayerPositionY], ax
    mov ah, 0x04
    int 16h
jmp checkUpSec


 ;----- SCANCODE FUNCTIONS -----

 ;Set the ISR 
init:
  push ax  

  mov ax, cs
  mov WORD [old_isr_15 + 02h], ax          
  ;old_isr_15 is now a far pointer to new_isr_15

  call swap_isr_15          ;Swap the current isr15 with the one in old_isr_15

  pop ax 
  ret

  ;Restore the original ISR 
dispose:
  call swap_isr_15          ;Swap the current isr15 with the one in old_isr_15                  

  ret  

  ;Swap the pointer in the IVT for int 15h with the pointer in old_isr_15
swap_isr_15:
  push eax
  push es 

  xor ax, ax 
  mov es, ax 

  cli

  mov eax, DWORD [es: 15h*4]
  xchg eax, DWORD [old_isr_15]
  mov DWORD [es: 15h*4], eax 

  sti

  pop es 
  pop eax

  ret  

  ;Wait for a change in the scancode table
wait_for_scancode:
 cli                           ;Prevent the ISR from messing things up 

 ;At least one scancode processed?
 cmp WORD [new_scancode], 0 
 jne _wfs_found                ;Yes

 ;No, restore interrupt so the CPU can process the prending ones
 sti
jmp wait_for_scancode

 ;New scancode, decrement the count and restore interrupts
_wfs_found:
 dec WORD [new_scancode]
 sti 

 ret

  ;THe BIOS is still saving keystrokes, we need to remove them or they 
  ;will fill the buffer up (should not be a big deal in theory).
remove_keystrokes:
 push ax

 ;Check if there are keystrokes to read.
 ;Release scancodes don't generate keystrokes 
_rk_try:
 mov ah, 01h 
 int 16h
 jz _rk_end      ;No keystrokes present, done 

 ;Some keystroke present, read it (won't block)
 xor ah, ah 
 int 16h
jmp _rk_try

_rk_end:
 pop ax 
 ret

 ;Tell if a scancode is pressed
 ;
 ;al = scancode  
 ;ZF clear is pressed 
is_scancode_pressed:
  push bx

  movzx bx, al 
  cmp BYTE [scancode_status + bx], 0 

  pop bx 
  ret 


prints:             ; mov si, string
  .loop:
    lodsb           ; bota character em al 
    cmp al, 0
    je .endloop
    call putchar
    jmp .loop
  .endloop:
ret

putchar:
  mov ah, 0x0e
  int 10h
ret

 ;AL = scancode 
new_isr_15:
 ;Check for right function
 cmp ah, 4fh
 jne _ni15_legacy

 ;Save used regs
 push bx
 push ax


 movzx bx, al            ;BX = scancode 
 and bl, 7fh             ;BX = scancode value

 sar al, 07h             ;AL = 0ffh if scancode has bit7 set (release), 00h otherwise
 not al                  ;AL = 00h if scancode has bit7 set (release), 0ffh otherwise

 ;Save the scancode status
 mov BYTE [cs:bx + scancode_status], al 
 ;Increment the count
 inc WORD [cs:new_scancode]

 pop ax
 pop bx 

_ni15_legacy:   
 ;This is a far jump, in NASM is simply jmp FAR [cs:old_isr_15]
 ;Ended up this way for debug
 push WORD [cs: old_isr_15 + 02h] 
 push WORD [cs: old_isr_15] 
 retf

  ;Original ISR
old_isr_15                      dw new_isr_15, 0  

 ;Scan code status table
scancode_status     TIMES 128   db 0
 ;Scan code count 
new_scancode                    dw 0
BITS 16
org 0x7c00  
jmp _main

_data:
    playerPositionY dw 0
    playerPositionX dw 0

_main:
    ; limpa alguns registradores
    xor ax, ax
    mov ds, ax
    mov cx, ax
    mov dx, ax

    call init 
    call setVideoMode

    ;posicao inicial da raquete
    mov word[playerPositionY], 75
    mov word[playerPositionX], 10

    mainLoop:

        ; call drawRacket
        
        ; mov ah, 0x01
        ; int 16h
        ; jz mainLoop
        ; mov ah, 00h
        ; int 16h
        ;Check if a is pressed 
        mov al, 1eh           ;a
        call is_scancode_pressed
        jz mainLoop

        call drawRacket

        ; ;Check if 'd' is pressed 
        ; mov al, 20h           ;d 
        ; call is_scancode_pressed
        ; jz mainLoop

        
    jmp mainLoop


jmp $

; inicializa o modo de video
setVideoMode: 
    mov ah, 0       ; primeiro parametro para chamar modo de video
    mov al, 13h     ; segundo parametro para chamar modo de video
    int 10h
ret

moveDown:
    mov ax, word[playerPositionY]
    
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
jmp mainLoop

moveUp:
    mov ax, word[playerPositionY]
    
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
jmp mainLoop

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

init:
  push ax  

  mov ax, cs
  mov WORD [old_isr_15 + 02h], ax        
  ;old_isr_15 is now a far pointer to new_isr_15

  call swap_isr_15          ;Swap the current isr15 with the one in old_isr_15

  pop ax 
ret

dispose:
  call swap_isr_15          ;Swap the current isr15 with the one in old_isr_15                  

ret

is_scancode_pressed:
  push bx

  movzx bx, al 
  cmp BYTE [scancode_status + bx], 0 

  pop bx 
ret

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


old_isr_15 dw new_isr_15, 0  
;Scan code status table
scancode_status times 128 db 0
;Scan code count 
new_scancode dw 0

times 510-($-$$) db 0
dw 0xaa55



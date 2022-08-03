org 0x7c00  
jmp _main

; _data:
playerPosition dw 0
; %1 == coluna (x)
; %2 == linha (y)
%macro drawRacket 2
    
    mov ah, 0ch ; coloca no modo de pintar pixels
    mov dx, %2

    for1:
        mov bx, 32 ;altura da raquete 
        add bx, %2
        cmp dx, bx  ; fim da raquete (linhas)
        je .fim1
        mov cx, %1  ; inicio da raquete (coluna)
        jmp .for2
        jmp for1

        .fim1:
        jmp sla

        ; laco for para varrer as 8 colunas da imagem
        .for2:
        mov bx, 8 ;largura da raquete
        add bx, %1
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

%endmacro


_main:
    ; limpa alguns registradores
    xor ax, ax
    mov ds, ax
    mov cx, ax
    mov dx, ax

    call setVideoMode

    ;posicao inicial da raquete
    mov word[playerPosition], 10

    mainLoop:

        drawRacket 10, word[playerPosition]
        
        sla:
        mov ah, 0x01
        int 16h
        cmp al, 115
        je moveDown
        
    jmp mainLoop


jmp $

; inicializa o modo de video
setVideoMode: 
    mov ah, 0       ; primeiro parametro para chamar modo de video
    mov al, 13h     ; segundo parametro para chamar modo de video
    int 10h
ret

moveDown:
    mov ax, word[playerPosition]
    inc ax
    mov [playerPosition], ax
    mov ah, 0x04
    int 16h
jmp mainLoop

times 510-($-$$) db 0
dw 0xaa55
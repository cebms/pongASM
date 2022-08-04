BITS 16
org 0x7c00

 call init 

 mov ax, 03h
 int 10h 

 
_main:
 call wait_for_scancode
 call draw_scancodes

_wait_for_keystroke:
 mov ah, 01h
 int 16h 
jz _main

 xor ah, ah
 int 16h 
 
 cmp al, 27d 
jne _main

 call dispose 

 mov ax, 4c00h
 int 21h


draw_scancodes:
 push es 
 push di 
 push si 
 push ax 
 push bx 
 push cx 
 
 mov si, strHeader 
 xor di, di 
 push 0b800h
 pop es 
 
 mov ah, 07h 
 xor bx, bx 
 call store_string
 
 mov di, 160d
 mov bx, 158d
 call store_string

 xor cx, cx 
 mov di, 166d 
 
_ds_draw:
 push di 

 mov bx, cx 
 shl bx, 04h 
 
__ds_scancode:
  
  mov al, 'X'
  and al, BYTE [scancode_status + bx]
 
  stosw
  add di, 02h
  
  inc bx 
  test bl, 0fh
 jnz __ds_scancode

 pop di 
 add di, 160d 
 
 inc cx 
 cmp cx, 08h
jb _ds_draw
 
 pop cx
 pop bx 
 pop ax 
 pop si 
 pop di 
 pop es 
 ret 

strHeader db "   0 1 2 3 4 5 6 7 8 9 a b c d e f", 0 
strLeft   db "01234567", 0  
 
store_string:
 lodsb 
 stosw 
 add di, bx
 test al, al 
 jnz store_string
 ret
 
init:
  push ax  
  
  mov ax, cs
  mov WORD [old_isr_15 + 02h], ax 
  call swap_isr_15
  
  pop ax 
  ret

dispose:
  call swap_isr_15
   
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
  
wait_for_scancode:
 cli 
 
 cmp WORD [new_scancode], 0 
 jne _wfs_found

 sti
jmp wait_for_scancode
 
_wfs_found:
 dec WORD [new_scancode]
 sti 
 
 ret

;al = scancode  
;ZF clear is pressed 
is_scancode_pressed:
  push bx
  movzx bx, al 
  cmp BYTE [scancode_status + bx], 0 
  pop bx 
  ret 
  
;AL = scancode 
new_isr_15:
 cmp ah, 4fh
 jne _ni15_legacy

 push bx
 push ax
 
 movzx bx, al
 and bl, 7fh 
 
 sar al, 07h
 not al 
 
 mov BYTE [cs:bx + scancode_status], al 
 inc WORD [cs:new_scancode]
 
 pop ax
 pop bx 
 
_ni15_legacy:   
 push WORD [cs: old_isr_15 + 02h] 
 push WORD [cs: old_isr_15] 
 retf
   
old_isr_15 						dw new_isr_15, 0  
scancode_status		TIMES 128 	db 0
new_scancode					dw 0

times 510-($-$$) db 0
dw 0xaa55
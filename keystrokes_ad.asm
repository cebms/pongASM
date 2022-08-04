; BITS 16
org 0x7c00



 ;Setup ISR for the scancode 
 call init 

 ;Clear screen
 mov ax, 03h
 int 10h

mov si, strCommand
call prints

 ;Print command
;  mov ah, 09h 
;  mov dx, strCommand
;  int 21h

_main:
 ;Wait for a change in the scancode tables
;  call wait_for_scancode
 ;Remove unused keystrokes
;  call remove_keystrokes

 ;Check if a is pressed 
 mov al, 1eh           ;a
 call is_scancode_pressed
 jz _main 

 ;Check if 'd' is pressed 
 mov al, 20h           ;d 
 call is_scancode_pressed
 jz _main 

 ;Both are pressed, print bye and ...
 mov si, strDone
 call prints
;  mov ah, 09h 
;  mov dx, strDone
;  int 21h 

 ;... restore the ISR and ...
;  call dispose 

 ;... exit
 mov ax, 4c00h
 int 21h

 strCommand db "Press 'a' and 'd' to exit", 13, 10, 24h, 0 
 strDone    db "Bye",13,10,13,10,24h, 0

 ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  L  
 ;  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll
 ;Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  Ll  L

 ;S C A N C O D E   F U N C T I O N S

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


times 510-($-$$) db 0
dw 0xaa55